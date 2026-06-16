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
    initFVPgen
  , processFVP
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
import           Term.LTerm (LNTerm, viewTerm, TermView(..), Lit(..), LVar(..), Name(..), NameTag(..), NameId(..), FunSym(..), ACSym(ACfct, Mult, Xor, Union, NatPlus))
import           Term.SubtermRule (CtxtStRule(..), StRhs(..), rRuleToCtxtStRule)
import           Term.Maude.Parser (parseReduceReply)
import           Term.Maude.Signature (MaudeSig, stFunSyms, stACFunSyms)
import           Term.Maude.Types (mTermToLNTerm)
import           Term.Rewriting.Definitions (RRule(RRule))
import           Control.Monad.Bind (evalBindT, noBindings)
import           Control.Monad.Fresh (evalFresh, nothingUsed)
import           Control.Exception (catch, SomeException)

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

-- | Wrapper pour initialiser la bibliothèque FVPgen
initFVPgen :: IO ()
initFVPgen = c_init_ocaml_runtime

-- | Appelle FVPgen avec une entrée FvpInput
processFVP :: FvpInput -> IO FvpResult
processFVP input = processFVPText (fvpInputToJsonString input)

-- | Version avec String directement
processFVPText :: String -> IO FvpResult
processFVPText input = do
    inputC <- newCString input
    resultC <- c_process_fvp_full_json inputC
    free inputC
    result <- peekCString resultC
    free resultC
    return (parseResultJson result)

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
    case L.dropWhile (/= ':') s of
        ':':rest ->
            let rest' = L.dropWhile (\c -> c == ' ' || c == '\t' || c == '\n') rest
            in case rest' of
                '"':more ->
                    let (val, _) = L.break (== '"') more
                    in val
                _ -> ""
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
      where extractNoEqSymbol (name, (arity, _, _)) = Symbol (BC.unpack name) arity "Syntactic"
    
    -- For ACfctSym: (name, (_, _)) - AC symbols are always binary
    acSymbols = map extractACfctSymbol (S.toList stACFunSymsSet)
      where extractACfctSymbol (name, (_, _)) = Symbol (BC.unpack name) 2 "AC"
    
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

-- | Build FVPInput from symbols and equations
-- Sets rewriteRn to empty and creates a void order (empty precedence)
buildFVPInput :: [Symbol] -> [(String, String)] -> FvpInput
buildFVPInput syms eqs = FvpInput
    { symbols = syms
    , order = Order (map symName syms)  -- alphabetical precedence
    , equations = eqs
    , rewriteRn = []    -- empty rewrite rules for now
    }

-- | Parse convergent_R from FVPResult back into CtxtStRule objects
-- Parses Maude-style rewrite rule strings (format: "lhs -> rhs") and converts them to CtxtStRule
-- Input: MaudeSig for parsing context, list of rule strings from FVPResult
-- Returns: Either error message or list of parsed CtxtStRule objects
parseConvergentRulesToCtxtStRule :: MaudeSig -> [String] -> Either String [CtxtStRule]
parseConvergentRulesToCtxtStRule _ [] = Right []
parseConvergentRulesToCtxtStRule maudeSig ruleStrs = 
    traverse parseOneRule ruleStrs
  where
    parseOneRule :: String -> Either String CtxtStRule
    parseOneRule ruleStr = do
        -- Split rule string by " -> " to get LHS and RHS
        case break (\c -> c == '-') ruleStr of
            (lhsStr, '-':'>':rhsStr) -> do
                -- Parse LHS and RHS using parseReduceReply
                let lhsBS = BC.pack (dropWhile (==' ') lhsStr)
                let rhsBS = BC.pack (dropWhile (==' ') rhsStr)
                
                -- parseReduceReply expects "result <sort>: <term>" format
                -- We need to wrap our terms in this format
                let lhsInput = BC.pack "result Msg: " <> lhsBS
                let rhsInput = BC.pack "result Msg: " <> rhsBS
                
                lhsMTerm <- parseReduceReply maudeSig lhsInput
                rhsMTerm <- parseReduceReply maudeSig rhsInput
                
                -- Convert MTerm to LNTerm using mTermToLNTerm with proper monad evaluation
                -- mTermToLNTerm "x" mt evaluates in BindT monad, we run it with empty bindings
                let lhsLNTerm = (mTermToLNTerm "x" lhsMTerm `evalBindT` noBindings) `evalFresh` nothingUsed :: LNTerm
                let rhsLNTerm = (mTermToLNTerm "x" rhsMTerm `evalBindT` noBindings) `evalFresh` nothingUsed :: LNTerm
                
                -- Create RRule and convert to CtxtStRule
                let rule = lhsLNTerm `RRule` rhsLNTerm
                case rRuleToCtxtStRule rule of
                    Just ctxtStRule -> Right ctxtStRule
                    Nothing -> Left $ "Failed to convert rule to CtxtStRule: " ++ ruleStr
            _ -> Left $ "Invalid rule format (expected 'lhs -> rhs'): " ++ ruleStr

-- | Backward compatibility: old name for parseConvergentRulesToCtxtStRule
parseConvergentRules :: [String] -> Either String [CtxtStRule]
parseConvergentRules _ = Left "parseConvergentRules requires MaudeSig context - use parseConvergentRulesToCtxtStRule instead"

-- | Main integration function: run full FVP pipeline from MaudeSig
-- Takes MaudeSig and equations, returns convergent rules or error
runFVPPipelineFromSig :: MaudeSig 
                      -> [CtxtStRule]
                      -> IO (Either String [CtxtStRule])
runFVPPipelineFromSig sig eqs = do
    let syms = collectAndSortSymbolsFromSig sig
    let acSyms = acSymbolNames syms
    let eqPairs = map (ctxtStRuleToPair acSyms) eqs
    runFVPPipeline sig syms eqPairs

-- | Main FVP pipeline: takes MaudeSig, symbols and equation pairs
-- MaudeSig is used for parsing convergent rules back to CtxtStRule objects
runFVPPipeline :: MaudeSig
               -> [Symbol] 
               -> [(String, String)] 
               -> IO (Either String [CtxtStRule])
runFVPPipeline maudeSig syms eqs = do
    let input = buildFVPInput syms eqs
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