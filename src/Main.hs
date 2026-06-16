-- |
-- Copyright   : (c) 2010, 2011 Benedikt Schmidt & Simon Meier
-- License     : GPL v3 (see LICENSE)
--
-- Portability : GHC only
--
-- Main module for the Tamarin prover with a GUI.
module Main where

import Main.Console          (defaultMain)
import Main.Mode.Batch       (batchMode)
import Main.Mode.Test        (testMode)
import Main.Mode.Interactive (interactiveMode)
import Main.Mode.Intruder    (intruderMode)

-- FVPgen initialization
import qualified Theory.Tools.CheckFiniteVariantProperty as FVP

main :: IO ()
main = do
  -- Initialize FVPgen library before anything else
  FVP.initFVPgen
  defaultMain batchMode [interactiveMode, intruderMode, testMode]
