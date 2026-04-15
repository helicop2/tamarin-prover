-- |
-- Copyright   : (c) 2024 Vincent Cheval, Robert Künnemann
-- License     : GPL v3 (see LICENSE)
--
-- Portability : GHC only (requires fvpgenbib library)
--
-- FVP Check via FVPgen library
------------------------------------------------------------------------------

{-# LANGUAGE ForeignFunctionInterface #-}

module Theory.Tools.CheckFiniteVariantProperty (
    initFVPgen
  , processFVP
  , processFVPText
  , FVPResult(..)
  , FVPInput(..)
  , FVPSymbol(..)
  , FVPOrder(..)
  ) where

import           Prelude                    hiding (id)
import           Foreign
import           Foreign.C.String
import           Foreign.Marshal.Alloc (free)

-- | Symbole pour FVPgen avec son nom, arité et catégorie (AC ou Syntactic)
data FVPSymbol = FVPSymbol
    { fvpSymbolName     :: String
    , fvpSymbolArity    :: Int
    , fvpSymbolCategory :: String
    } deriving (Show, Eq)

-- | Ordre de précédence entre symboles
data FVPOrder = FVPOrder
    { fvpPrecedence :: [String] }
    deriving (Show, Eq)

-- | Entrée FVP contenant symbols, ordre, équations et règles optionnelles
data FVPInput = FVPInput
    { fvpSymbols     :: [FVPSymbol]
    , fvpOrder      :: FVPOrder
    , fvpEquations  :: [(String, String)]
    , fvpInputRewriteRn :: [(String, String)]
    } deriving (Show, Eq)

-- | Résultat FVP contenant les règles et le statut de convergence
data FVPResult = FVPResult
    { fvpResultRewriteR      :: [String]
    , fvpResultRewriteRn    :: [String]
    , fvpEquationsEn  :: [String]
    , fvpConvergence :: String
    , fvpRuleCount   :: Int
    , fvpConvergentR  :: [String]
    } deriving (Show, Eq)

-- | Initialise le runtime OCaml (doit être appelé une fois au démarrage)
foreign import ccall "bridge.h init_ocaml_runtime"
    c_init_ocaml_runtime :: IO ()

-- | Fonction principale de traitement FVP
foreign import ccall "bridge.h process_fvp_full_json"
    c_process_fvp_full_json :: CString -> IO CString

-- | Wrapper pour initialiser la bibliothèque FVPgen
initFVPgen :: IO ()
initFVPgen = c_init_ocaml_runtime

-- | Appelle FVPgen avec une entrée JSON
processFVP :: FVPInput -> IO FVPResult
processFVP input = processFVPText (fvpInputToJson input)

-- | Conversion FVPInput vers JSON string
fvpInputToJson :: FVPInput -> String
fvpInputToJson (FVPInput syms ord eqs rn) =
    "{" ++
    "\"symbols\":" ++ symbolsToJson syms ++ "," ++
    "\"order\":" ++ orderToJson ord ++ "," ++
    "\"equations\":" ++ equationsToJson eqs ++ "," ++
    "\"rewriteRn\":" ++ equationsToJson rn ++ "," ++
    "\"rewriteRnConvergent\":true" ++
    "}"

-- | Version avec String directement
processFVPText :: String -> IO FVPResult
processFVPText input = do
    inputC <- newCString input
    resultC <- c_process_fvp_full_json inputC
    free inputC
    result <- peekCString resultC
    free resultC
    return (parseFVPResult result)

-- | Parse le résultat JSON manuellement
parseFVPResult :: String -> FVPResult
parseFVPResult s = FVPResult
    { fvpResultRewriteR = extractArray s "rewrite_R"
    , fvpResultRewriteRn = extractArray s "rewrite_Rn"
    , fvpEquationsEn = extractArray s "equations_En"
    , fvpConvergence = extractString s "convergence"
    , fvpRuleCount = extractInt s "rule_count"
    , fvpConvergentR = extractArray s "convergent_R"
    }

-- | Helpers JSON
symbolsToJson :: [FVPSymbol] -> String
symbolsToJson syms = "[" ++ go syms ++ "]"
  where
    go [] = ""
    go [s] = symbolToJson s
    go (s:ss) = symbolToJson s ++ "," ++ go ss

symbolToJson :: FVPSymbol -> String
symbolToJson (FVPSymbol n a c) =
    "{\"name\":\"" ++ n ++ "\",\"arity\":" ++ show a ++ ",\"category\":\"" ++ c ++ "\"}"

orderToJson :: FVPOrder -> String
orderToJson (FVPOrder ps) = "{\"precedence\":[" ++ go ps ++ "]}"
  where
    go [] = ""
    go [s] = "\"" ++ s ++ "\""
    go (s:ss) = "\"" ++ s ++ "\"," ++ go ss

equationsToJson :: [(String, String)] -> String
equationsToJson eqs = "[" ++ go eqs ++ "]"
  where
    go [] = ""
    go [(l,r)] = "[\"" ++ l ++ "\",\"" ++ r ++ "\"]"
    go ((l,r):rest) = "[\"" ++ l ++ "\",\"" ++ r ++ "\"]," ++ go rest

extractArray :: String -> String -> [String]
extractArray json key = go (searchKey json key)
  where
    go s = case dropWhile (\c -> c /= '[') s of
        '[' : rest -> parseStringArray rest
        _ -> []
    parseStringArray s = case dropWhile (\c -> c /= '"') s of
        '"' : rest ->
            let (val, rest') = break (== '"') rest
            in val : case dropWhile (\c -> c /= ',' && c /= ']') rest' of
                ',' : more -> parseStringArray more
                ']' : _ -> []
                _ -> []
        _ -> []

extractString :: String -> String -> String
extractString json key = go (searchKey json key)
  where
    go s = case dropWhile (\c -> c /= ':') s of
        ':' : rest ->
            let rest' = dropWhile (\c -> c == ' ' || c == '\t' || c == '\n') rest
            in case rest' of
                '"' : more ->
                    let (val, rest'') = break (== '"') more
                    in val
                _ -> ""
        _ -> ""

extractInt :: String -> String -> Int
extractInt json key = go (searchKey json key)
  where
    go s = case dropWhile (\c -> c /= ':') s of
        ':' : rest ->
            let rest' = dropWhile (\c -> c == ' ' || c == '\t' || c == '\n') rest
                nums = takeWhile (\c -> c >= '0' && c <= '9') rest'
            in case reads nums of
                (n, _) : _ -> n
                _ -> 0
        _ -> 0

searchKey :: String -> String -> String
searchKey json key = go json
  where
    keyLen = length key
    go s@('"' : rest) =
        if take keyLen rest == key
            then drop keyLen rest
            else go (tail s)
    go (_ : rest) = go rest
    go [] = []