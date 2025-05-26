module Main where

import HHC.Repl
import HHC.CodeGen.LLVM.Test qualified as T

main :: IO ()
main = T.main
