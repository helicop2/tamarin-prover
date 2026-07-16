-- |
-- Copyright   : (c) 2024 Vincent Cheval, Robert Künnemann
-- License     : GPL v3 (see LICENSE)
--
-- Portability : GHC only (requires fvpgenbib library)
--
-- FVP Check via FVPgen library
-- Uses types and functions from fvpgenbib/bridge/rewrite_rule_json.hs
------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Theory.Tools.CheckFiniteVariantProperty (
    processFVP
  , processFVPText
  , fvpInputToJsonString
  , parseResultJson
  , Symbol(..)
  , Order(..)
  , FvpInput(..)
  , FvpResult(..)
  , ProjectionMapping
  -- Helper functions for Tamarin integration
  , collectAndSortSymbolsFromSig
  , ctxtStRuleToPair
  , lnTermToPrefixString
  , acSymbolNames
  , buildFVPInput
  , parseConvergentRules
  , parseConvergentRulesToCtxtStRule
  , runFVPPipelineFromSig
  , buildProjectionMapping
  ) where

import           Prelude                    hiding (id)
import           Foreign hiding (Xor)
import           Foreign.C.String
import qualified Data.List           as L
import qualified Data.Set            as S
import qualified Data.Map            as M
import qualified Data.ByteString.Char8 as BC
import           System.IO (hFlush, stdout)
import           Term.LTerm (LNTerm, viewTerm, TermView(..), Lit(..), LVar(..), LSort(..), Name(..), NameTag(..), NameId(..), FunSym(..), ACSym(ACfct, Mult, Xor, Union, NatPlus), Term(FAPP, LIT))
import           Term.SubtermRule (CtxtStRule(..), StRhs(..), rRuleToCtxtStRule)
import           Term.Maude.Signature (MaudeSig, stFunSyms, stACFunSyms)
import           Term.Rewriting.Definitions (RRule(RRule))
import           Control.Concurrent (forkOS)
import           Control.Monad (forever)
import           Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import           Control.Concurrent.MVar (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import           Control.Exception (catch, try, throwIO, SomeException)
import           System.IO.Unsafe (unsafePerformIO)
import           Data.Char (isAlphaNum, isDigit, isUpper, isSpace)

-- =============================================================================
-- Types (from fvpgenbib/bridge/rewrite_rule_json.hs)
-- =============================================================================

-- | Symbole avec son nom, arité et catégorie (AC ou Syntactic)
data Symbol = Symbol
    { symName     :: String
    , symArity    :: Int
    , symCategory :: String  -- "AC" ou "Syntactic"
    } deriving (Show)

-- | Ordre de précédence entre symboles
data Order = Order
    { precedence :: [String] }
    deriving (Show)

-- | Entree principale contenant symbols, ordre, equations et regles optionnelles
data FvpInput = FvpInput
    { symbols     :: [Symbol]
    , order       :: Order
    , equations   :: [(String, String)]
    , rewriteRn   :: [(String, String)]
    } deriving (Show)

-- | Résultat FVP contenant les regles et le statut de convergence
data FvpResult = FvpResult
    { rewriteR    :: [String]
    , rewriteRnRslt :: [String]  -- renamed to avoid conflict with FvpInput
    , equationsEn :: [String]
    , convergence :: String
    , ruleCount   :: Int
    , convergentR :: [String]
    } deriving (Show)

-- =============================================================================
-- FFI imports (from bridge.h)
-- =============================================================================

-- | Initialise le runtime OCaml (doit être appelé une fois au démarrage)
foreign import ccall "bridge.h init_ocaml_runtime"
    c_init_ocaml_runtime :: IO ()

-- | Fonction principale de traitement FVP
foreign import ccall "bridge.h process_fvp_full_json"
    c_process_fvp_full_json :: CString -> IO CString

-- | Channel for dispatching work to the dedicated FVP engine thread.
--   The engine thread is created on demand by 'ensureFvpEngine' on a
--   bound OS thread via 'forkOS', ensuring all OCaml FFI calls share the
--   same OS thread and thus the same OCaml domain state.
{-# NOINLINE globalFvpChan #-}
globalFvpChan :: MVar (Maybe (Chan (IO ())))
globalFvpChan = unsafePerformIO $ newMVar Nothing

-- | Ensure the FVP engine thread is running; returns the dispatch channel.
--   Lazily creates the bound thread on first call via 'modifyMVar' for
--   thread-safe single initialisation.
ensureFvpEngine :: IO (Chan (IO ()))
ensureFvpEngine = modifyMVar globalFvpChan $ \m -> case m of
    Just chan -> return (Just chan, chan)
    Nothing   -> do
        chan <- newChan
        _ <- forkOS $ do
            c_init_ocaml_runtime
            putStrLn "[FVPgen] Engine ready on bound thread"
            hFlush stdout
            forever $ readChan chan >>= \x -> x
        return (Just chan, chan)

-- | Appelle FVPgen avec une entrée FvpInput
processFVP :: FvpInput -> IO FvpResult
processFVP input = processFVPText (fvpInputToJsonString input)

-- | Version avec String directement
--   Dispatches work to the dedicated FVP engine thread so that all
--   OCaml FFI calls ('c_process_fvp_full_json') run on the same OS thread
--   that called 'caml_startup', avoiding OCaml 5.x domain-lock assertion.
--   The engine thread is lazily created on the first call.
processFVPText :: String -> IO FvpResult
processFVPText input = do
    chan <- ensureFvpEngine
    resultVar <- (newEmptyMVar :: IO (MVar (Either SomeException FvpResult)))
    writeChan chan $ do
        r <- try $ withCString input $ \inputC -> do
            resultC <- c_process_fvp_full_json inputC
            s <- peekCString resultC
            free resultC
            return $ parseResultJson s
        putMVar resultVar r
    takeMVar resultVar >>= either throwIO return

-- =============================================================================
-- JSON Conversion (from fvpgenbib/bridge/rewrite_rule_json.hs)
-- =============================================================================

-- | Convertit Symbol en JSON
symbolToJson :: Symbol -> String
symbolToJson (Symbol n a c) = 
    "{\"name\":\"" ++ n ++ "\",\"arity\":" ++ show a ++ ",\"category\":\"" ++ c ++ "\"}"

-- | Convertit liste de Symbol en JSON array
symbolsToJson :: [Symbol] -> String
symbolsToJson syms = "[" ++ go syms ++ "]"
  where
    go [] = ""
    go [s] = symbolToJson s
    go (s:ss) = symbolToJson s ++ "," ++ go ss

-- | Convertit Order en JSON
orderToJson :: Order -> String
orderToJson (Order ps) = 
    "{\"precedence\":[" ++ go ps ++ "]}"
  where
    go [] = ""
    go [s] = "\"" ++ s ++ "\""
    go (s:ss) = "\"" ++ s ++ "\"," ++ go ss

-- | Convertit liste de tuples (equations) en JSON
equationsToJson :: [(String, String)] -> String
equationsToJson eqs = "[" ++ go eqs ++ "]"
  where
    go [] = ""
    go [(l,r)] = "[\"" ++ l ++ "\",\"" ++ r ++ "\"]"
    go ((l,r):rest) = "[\"" ++ l ++ "\",\"" ++ r ++ "\"]," ++ go rest

-- | Transforme FvpInput en JSON string
fvpInputToJsonString :: FvpInput -> String
fvpInputToJsonString (FvpInput syms ord eqs rn) =
    "{" ++
    "\"symbols\":" ++ symbolsToJson syms ++ "," ++
    "\"order\":" ++ orderToJson ord ++ "," ++
    "\"equations\":" ++ equationsToJson eqs ++ "," ++
    "\"rewriteRn\":" ++ equationsToJson rn ++ "," ++
    "\"rewriteRnConvergent\":true" ++
    "}"

-- =============================================================================
-- JSON Result Parsing (from fvpgenbib/bridge/rewrite_rule_json.hs)
-- =============================================================================

-- | Cherche une clé et retourne le reste du JSON à partir de cette position
findKeyInJson :: String -> String -> String
findKeyInJson key json = 
    go json
  where
    go [] = ""
    go s@('"':xs) = 
        if key `L.isPrefixOf` takeQuoted xs
            then dropUntilValue s
            else go xs
    go (_:xs) = go xs
    
    takeQuoted s = L.takeWhile (/= '"') s
    dropUntilValue s = L.drop (L.length key + 3) s  -- Skip "key": -- FIX: +3 not +2

-- | Extrait une valeur array d'une position donnée
extractArrayAt :: String -> [String]
extractArrayAt s = 
    case L.dropWhile (\c -> c /= '[') s of
        '[':xs -> parseJsonArray xs []
        _ -> []

-- | Parse un array JSON
parseJsonArray :: String -> [String] -> [String]
parseJsonArray s acc = 
    case s of
        ']':_ -> reverse acc
        ',':r -> parseJsonArray r acc
        '"':xs -> 
            case L.break (== '"') xs of
                (val, '"':rest) -> parseJsonArray rest (val : acc)
                _ -> reverse acc
        _ -> reverse acc

-- | Extrait une valeur number d'une position donnée
extractNumberAt :: String -> String
extractNumberAt s = 
    let s' = L.dropWhile (\c -> not (c >= '0' && c <= '9')) s
        nums = L.takeWhile (\c -> c >= '0' && c <= '9') s'
    in nums

-- | Parse le JSON résultat
parseResultJson :: String -> FvpResult
parseResultJson json = 
    let rwR = extractArrayAt (findKeyInJson "rewrite_R" json)
        rwRn = extractArrayAt (findKeyInJson "rewrite_Rn" json)
        eqEn = extractArrayAt (findKeyInJson "equations_En" json)
        conv = extractStringAt (findKeyInJson "convergence" json)
        cnt = extractNumberAt (findKeyInJson "rule_count" json)
        convR = extractArrayAt (findKeyInJson "convergent_R" json)
    in FvpResult
        { rewriteR = rwR
        , rewriteRnRslt = rwRn
        , equationsEn = eqEn
        , convergence = conv
        , ruleCount = if L.null cnt then 0 else read cnt
        , convergentR = convR
        }

-- =============================================================================
-- Helper functions
-- =============================================================================

-- | Extrait une valeur string d'une position donnée (helper)
extractStringAt :: String -> String
extractStringAt s =
    let trimmed = L.dropWhile (\c -> c == ' ' || c == '\t' || c == '\n') s
    in case trimmed of
        '"':more ->
            let (val, _) = L.break (== '"') more
            in val
        _ -> ""

-- =============================================================================
-- Tamarin Integration Functions
-- =============================================================================

-- | Extract names of AC symbols from symbol list
acSymbolNames :: [Symbol] -> S.Set String
acSymbolNames syms = S.fromList [symName s | s <- syms, symCategory s == "AC"]

-- | Type alias for projection variable mapping (kept for API compatibility)
type ProjectionMapping = M.Map String String

-- | Build a simple empty projection mapping (kept for API compatibility)
buildProjectionMapping :: LNTerm -> LNTerm -> ProjectionMapping
buildProjectionMapping _ _ = M.empty


funSymName :: FunSym -> String
funSymName (NoEq (f, _))        = BC.unpack f
funSymName (AC (ACfct (f, _))) = BC.unpack f
funSymName (C op)               = show op
funSymName List                 = "LIST"
funSymName (AC Mult)            = "mult"
funSymName (AC Xor)             = "xor"
funSymName (AC Union)           = "union"
funSymName (AC NatPlus)         = "tplus"

-- | Convert LNTerm to prefix notation string (FVPgenbib format)
-- Variable names with .N notation (e.g., x.1) are output as x_N to preserve
-- the distinction between pair components naturally from the parser's lvarIdx.
--
-- Key formatting rules:
--   * Function applications: f(x,y) NOT f(x, y) (no spaces after commas)
--   * AC symbols: Binary nesting f(f(x,y),z) NOT f(x,y,z) NOT f(x,y)
--   * Variables: lvarName + "_" + lvarIdx if idx > 0, otherwise plain name
--   * Constants: quoted form (e.g., '0, ~'1, #'2, %'3)
--   * 0-ary symbols: bare name without parentheses
lnTermToPrefixString :: S.Set String -> ProjectionMapping -> LNTerm -> String
lnTermToPrefixString acSyms _projMapping t = case viewTerm t of
    Lit (Var lv) ->
        let baseName = lvarName lv
            idx = lvarIdx lv
        in if idx > 0
           then baseName ++ "_" ++ show idx
           else baseName
    Lit (Con (Name tag (NameId nid))) -> case tag of
        FreshName -> "~'" ++ show nid
        PubName   -> "'" ++ show nid
        NodeName  -> "#'" ++ show nid
        NatName   -> "%'" ++ show nid
    FApp fsym args ->
        let fname = funSymName fsym
            isAC = S.member fname acSyms
            argsStrs = map (lnTermToPrefixString acSyms _projMapping) args
        in if length argsStrs == 0
           then fname  -- 0-ary symbol: bare name, no parentheses
           else if isAC
                then flattenACArgs fname argsStrs  -- returns nested expr directly
                else fname ++ "(" ++ L.intercalate "," argsStrs ++ ")"

-- | Flatten AC arguments to binary nesting (FVPgenbib requirement)
-- AC symbols must be binary: f(x,y,z) becomes f(f(x,y),z)
-- Returns the fully nested expression string directly
flattenACArgs :: String -> [String] -> String
flattenACArgs fname args = case args of
    [] -> error "flattenACArgs: cannot flatten empty args"
    [a] -> a
    (a1:a2:rest) -> nestBinary fname (a1:a2:rest)
  where
    nestBinary :: String -> [String] -> String
    nestBinary f (a1:a2:rest) = f ++ "(" ++ a1 ++ "," ++ nestBinary f (a2:rest) ++ ")"
    nestBinary _ [a] = a
    nestBinary _ [] = error "flattenACArgs: unexpected empty list"

-- | Extract symbols from MaudeSig and convert to FVPgen Symbol list
-- Returns symbols sorted alphabetically by name
-- This function works directly with MaudeSig to avoid exposing internal types
collectAndSortSymbolsFromSig :: MaudeSig -> [Symbol]
collectAndSortSymbolsFromSig sig = 
    L.sortBy (\s1 s2 -> compare (symName s1) (symName s2)) allSymbols
  where
    -- NoEqSym = (ByteString, (Int, Privacy, Constructability))
    -- ACfctSym = (ByteString, (Privacy, Constructability))
    stFunSymsSet = stFunSyms sig
    stACFunSymsSet = stACFunSyms sig
    
    -- Extract symbols from the sets using pattern matching on tuples
    -- For NoEqSym: (name, (arity, _, _))
    noEqSymbols = map extractNoEqSymbol (S.toList stFunSymsSet)
      where extractNoEqSymbol (name, (arity, _, _, _)) = Symbol (BC.unpack name) arity "Syntactic"
    
    -- For ACfctSym: (name, (_, _)) - AC symbols are always binary
    acSymbols = map extractACfctSymbol (S.toList stACFunSymsSet)
      where extractACfctSymbol (name, (_, _, _)) = Symbol (BC.unpack name) 2 "AC"
    
    allSymbols = noEqSymbols ++ acSymbols

-- | Convert CtxtStRule to (String, String) pair using prefix notation
-- First element is left-hand side, second is right-hand side
-- Requires AC symbol set for proper binary nesting conversion
-- Automatically builds and applies projection variable mapping for consistency
ctxtStRuleToPair :: S.Set String -> CtxtStRule -> (String, String)
ctxtStRuleToPair acSyms (CtxtStRule lhs (StRhs _ rhs)) =
    let projMapping = buildProjectionMapping lhs rhs
        lhsStr = lnTermToPrefixString acSyms projMapping lhs
        rhsStr = lnTermToPrefixString acSyms projMapping rhs
    in (lhsStr, rhsStr)

-- | Build FVPInput from symbols, equations, and user-defined precedence.
-- The precedence list defines the reduction order used by FVPgen.
buildFVPInput :: [Symbol] -> [(String, String)] -> [String] -> FvpInput
buildFVPInput syms eqs precedence = FvpInput
    { symbols = syms
    , order = Order precedence
    , equations = eqs
    , rewriteRn = []    -- empty rewrite rules for now
    }

-- | Parse convergent_R from FVPResult back into CtxtStRule objects
-- Parses OCaml prefix-format rewrite rule strings (e.g. "f(X_1,X_2) -> X_1")
-- and converts them to CtxtStRule. The OCaml output uses X_N for variables and
-- f(args) for syntactic function applications.
parseConvergentRulesToCtxtStRule :: MaudeSig -> [String] -> Either String [CtxtStRule]
parseConvergentRulesToCtxtStRule _ [] = Right []
parseConvergentRulesToCtxtStRule maudeSig ruleStrs = do
    let symMap = buildFunSymMap maudeSig
    rrules <- traverse (parseOneRule symMap) ruleStrs
    traverse ruleToCtxtStRule rrules
  where
    parseOneRule symMap ruleStr = do
        case splitArrow ruleStr of
            Just (lhsStr, rhsStr) -> do
                lhs <- parseOCamlTerm symMap lhsStr
                rhs <- parseOCamlTerm symMap rhsStr
                Right (lhs `RRule` rhs)
            Nothing -> Left $ "Invalid rule format (expected 'lhs -> rhs'): " ++ ruleStr

    ruleToCtxtStRule rrule =
        case rRuleToCtxtStRule rrule of
            Just ctxtStRule -> Right ctxtStRule
            Nothing -> Left $ "Failed to convert rule to CtxtStRule: " ++ show rrule

-- | Backward compatibility: old name for parseConvergentRulesToCtxtStRule
parseConvergentRules :: [String] -> Either String [CtxtStRule]
parseConvergentRules _ = Left "parseConvergentRules requires MaudeSig context - use parseConvergentRulesToCtxtStRule instead"

-- | Build a map from function symbol name to (arity, FunSym) for OCaml term parsing
buildFunSymMap :: MaudeSig -> M.Map String (Int, FunSym)
buildFunSymMap sig =
    M.fromList noEqEntries `M.union` M.fromList acEntries
  where
    noEqEntries =
        [ (BC.unpack name, (arity, NoEq (name, (arity, priv, constr, ndc))))
        | (name, (arity, priv, constr, ndc)) <- S.toList (stFunSyms sig)
        ]
    acEntries =
        [ (BC.unpack name, (2, AC (ACfct (name, (priv, constr, ndc)))))
        | (name, (priv, constr, ndc)) <- S.toList (stACFunSyms sig)
        ]

-- | Split a rule string on \" -> \"
splitArrow :: String -> Maybe (String, String)
splitArrow s =
    case break (== '-') s of
        (lhs, '-':'>':rhs) -> Just (lhs, rhs)
        _                  -> Nothing

-- | Parse a single OCaml prefix-format term into LNTerm
-- Handles: f(X_1,X_2), X_1, c (0-ary), and parenthesized subterms
parseOCamlTerm :: M.Map String (Int, FunSym) -> String -> Either String LNTerm
parseOCamlTerm symMap input =
    case parseTerm' (dropWhile isSpace input) of
        Right (t, rest) ->
            case parseInfixCont t (dropWhile isSpace rest) of
                Right (t', rest') ->
                    if null (dropWhile isSpace rest')
                        then Right t'
                        else Left $ "Trailing characters in term: " ++ take 20 rest'
                Left err -> Left err
        Left err -> Left err
  where
    parseTerm' s
        | null s = Left "Unexpected end of term"
        | head s == '(' = do
            let (inner, afterParen) = extractParenBlock (tail s)
            (t, rest) <- parseTerm' (dropWhile isSpace inner)
            case parseInfixCont t (dropWhile isSpace rest) of
                Right (t', rest') ->
                    if null (dropWhile isSpace rest')
                        then Right (t', afterParen)
                        else Left $ "Trailing content in parenthesized expression: " ++ take 20 rest'
                Left err -> Left err
        | otherwise =
            let (name, rest) = span (\c -> isAlphaNum c || c == '_') s
            in if null name
               then Left $ "Unexpected character: " ++ take 20 s
               else case dropWhile isSpace rest of
                   '(' : afterOpen -> do
                       let (inner, afterParen) = extractParenBlock afterOpen
                       args <- parseArgs (dropWhile isSpace inner)
                       case M.lookup name symMap of
                           Just (arity, fsym)
                               | length args == arity ->
                                   Right (FAPP fsym args, afterParen)
                               | otherwise ->
                                   Left $ "Arity mismatch for " ++ name ++ ": expected " ++ show arity ++ ", got " ++ show (length args)
                           Nothing ->
                               Left $ "Unknown function: " ++ name
                   rest' ->
                       case M.lookup name symMap of
                           Just (0, fsym) -> Right (FAPP fsym [], rest')
                           Just (arity, _) ->
                               Left $ "Function " ++ name ++ " requires " ++ show arity ++ " arguments"
                           Nothing -> case parseOCamlVar name of
                               Just (vname, vidx) ->
                                   Right (LIT (Var (LVar vname LSortMsg vidx)), rest')
                               Nothing ->
                                   Left $ "Unknown identifier: " ++ name

    parseArgs s
        | null s = Right []
        | head s == ')' = Right []
        | otherwise = case parseTerm' s of
            Right (t, rest) -> case dropWhile isSpace rest of
                ',' : more -> (t:) <$> parseArgs (dropWhile isSpace more)
                _          -> Right [t]
            Left err -> Left err

    -- | Try to continue parsing an infix AC expression.
    -- After parsing a term @t@, if the remaining input starts with a known
    -- binary AC operator followed by another term, chain them as FAPP.
    -- Recurse to handle chained infix (e.g. a op b op c).
    parseInfixCont :: LNTerm -> String -> Either String (LNTerm, String)
    parseInfixCont t s =
        let (op, rest2) = span (\c -> isAlphaNum c || c == '_') s
        in if null op
           then Right (t, s)
           else case M.lookup op symMap of
               Just (2, fsym) ->
                   case parseTerm' (dropWhile isSpace rest2) of
                       Right (right, rest4) ->
                           parseInfixCont (FAPP fsym [t, right]) (dropWhile isSpace rest4)
                       Left err -> Left err
               _ -> Right (t, s)

-- | Extract content between matching parentheses.
-- Input: string AFTER the opening '('
-- Returns: (content_between_parens, string_after_matching_')')
extractParenBlock :: String -> (String, String)
extractParenBlock = go (0 :: Integer) ""
  where
    go _ acc "" = (reverse acc, "")
    go 0 acc (')' : rest) = (reverse acc, rest)
    go n acc (')' : rest) = go (n - 1) (')' : acc) rest
    go n acc ('(' : rest) = go (n + 1) ('(' : acc) rest
    go n acc (c : rest)   = go n (c : acc) rest

-- | Parse OCaml variable name like X_1, X_2 etc.
-- Returns (base_name, index) or Nothing if not a valid variable
parseOCamlVar :: String -> Maybe (String, Integer)
parseOCamlVar s = case s of
    (ch : _) | isUpper ch ->
        let (base, rest) = span (\ch -> isAlphaNum ch || ch == '_') s
        in case rest of
            '_' : numStr
                | not (null numStr), all isDigit numStr ->
                    Just (base, read numStr)
            "" -> Just (base, 0)
            _  -> Nothing
    _ -> Nothing

-- | Main integration function: run full FVP pipeline from MaudeSig
-- Takes MaudeSig, equations, and a user-defined function precedence.
-- Builds the full precedence by appending remaining (builtin) symbols
-- alphabetically after the user-defined order.
runFVPPipelineFromSig :: MaudeSig
                      -> [CtxtStRule]
                      -> [String]
                      -> IO (Either String [CtxtStRule])
runFVPPipelineFromSig sig eqs userPrecedence = do
    let syms = collectAndSortSymbolsFromSig sig
        acSyms = acSymbolNames syms
        eqPairs = map (ctxtStRuleToPair acSyms) eqs
        -- Build full precedence: user-defined order first, then remaining builtins alphabetically
        allNames = map symName syms
        remainingNames = L.sort $ filter (`notElem` userPrecedence) allNames
        fullPrecedence = userPrecedence ++ remainingNames
    runFVPPipeline sig syms eqPairs fullPrecedence

-- | Main FVP pipeline: takes MaudeSig, symbols, equation pairs and precedence.
-- MaudeSig is used for parsing convergent rules back to CtxtStRule objects
runFVPPipeline :: MaudeSig
               -> [Symbol]
               -> [(String, String)]
               -> [String]
               -> IO (Either String [CtxtStRule])
runFVPPipeline maudeSig syms eqs precList = do
    let input = buildFVPInput syms eqs precList
    -- DEBUG: Print FVPInput before sending to FVPgen
    putStrLn "[DEBUG FVP] FVPInput being sent to FVPgen:"
    putStrLn $ "[DEBUG FVP] Symbols (" ++ show (length syms) ++ "):"
    mapM_ (\s -> putStrLn $ "  - " ++ symName s ++ "/" ++ show (symArity s) ++ " (" ++ symCategory s ++ ")") syms
    putStrLn $ "[DEBUG FVP] Order precedence (" ++ show (length (precedence (order input))) ++ "):"
    mapM_ (\s -> putStrLn $ "  - " ++ s) (precedence (order input))
    putStrLn $ "[DEBUG FVP] Equations (" ++ show (length eqs) ++ "):"
    mapM_ (\(l,r) -> putStrLn $ "  - " ++ l ++ " = " ++ r) eqs
    putStrLn "[DEBUG FVP] ----------------------------------------"
    hFlush stdout
    result <- (processFVP input) `catch` handleException
    putStrLn $ "[DEBUG FVP] Raw FVPgen result JSON: " ++ show result
    case result of
        FvpResult _ _ _ conv _ convR -> 
            case conv of
                "yes" -> do
                    -- FVP check passed - parse convergent rules
                    case parseConvergentRulesToCtxtStRule maudeSig convR of
                        Right rules -> return $ Right rules
                        Left err -> return $ Left $ "Failed to parse convergent rules: " ++ err
                _ -> return $ Left $ "FVP check failed with convergence: " ++ conv
  where
    handleException :: SomeException -> IO FvpResult
    handleException ex = 
        return $ FvpResult [] [] [] ("error: " ++ show ex) 0 []