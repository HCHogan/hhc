{-# OPTIONS_GHC -fsimpl-tick-factor=500 #-}
{- ToDo: remove simplifier ticket option, cf. LLVM.Util.Memory -}
module Main (main) where

import LLVM.Util.Loop (forLoop)
import LLVM.Util.Optimize (optimizeModule)
import LLVM.Core

import Control.Monad (foldM, void)
import Data.Word (Word, Word32)


cg :: CodeGenModule (Function (Double -> IO (Ptr Double)))
cg = do
    dotProd <- createFunction InternalLinkage $ \ size aPtr aStride bPtr bStride -> do
        r <- forLoop (valueOf 0) size (valueOf 0) $ \ i s -> do
            ai <- mul aStride i
            bi <- mul bStride i
            ap <- getElementPtr aPtr (ai, ())
            bp <- getElementPtr bPtr (bi, ())
            a <- load ap
            b <- load bp
            ab <- mul a b
            add (s :: Value Double) ab
        ret r
    let _ = dotProd :: Function (Word32 -> Ptr Double -> Word32 -> Ptr Double -> Word32 -> IO Double)

    -- multiply a:[n x m], b:[m x l]
    matMul <- createFunction InternalLinkage $ \ n m l aPtr bPtr cPtr -> do
        forLoop (valueOf 0) n () $ \ ni () -> do
           forLoop (valueOf 0) l () $ \ li () -> do
              ni' <- mul ni m
              row <- getElementPtr aPtr (ni', ())
              col <- getElementPtr bPtr (li, ())
              x <- call dotProd m row (valueOf 1) col m
              j <- add ni' li
              p <- getElementPtr cPtr (j, ())
              store x p
        ret ()
    let _ = matMul :: Function (Word32 -> Word32 -> Word32 -> Ptr Double -> Ptr Double -> Ptr Double -> IO ())

    let fillArray =
            (void .) .
            foldM (\ptr x -> store x ptr >> getElementPtr ptr (1::Word32,()))

    test <- createNamedFunction ExternalLinkage "test" $ \ x -> do
        a <- arrayMalloc (4 :: Word)
        fillArray a $ map valueOf [1,2,3,4]
        b <- arrayMalloc (4 :: Word)
        fillArray b [x,x,x,x]
        c <- arrayMalloc (4 :: Word)
        _ <- call matMul (valueOf 2) (valueOf 2) (valueOf 2) a b c
        ret c
    let _ = test :: Function (Double -> IO (Ptr Double))

    return test

main :: IO ()
main = do
    -- Initialize jitter
    initializeNativeTarget
    m <- createModule $ setTarget hostTriple >> cg >> getModule
    writeBitcodeToFile "Arr.bc" m
    _ <- optimizeModule 3 m
    writeBitcodeToFile "Arr-opt.bc" m
