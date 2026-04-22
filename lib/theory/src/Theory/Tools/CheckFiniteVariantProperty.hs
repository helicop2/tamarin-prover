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
  ) where

import           Prelude                    hiding (id)
import           Foreign
import           Foreign.C.String
import           Foreign.Marshal.Alloc (free)
import qualified Data.List           as L

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
                (val, _) -> val
                _ -> ""
        _ -> ""

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