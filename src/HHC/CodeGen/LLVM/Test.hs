module HHC.CodeGen.LLVM.Test (main) where

import Data.Int (Int32)
import LLVM.Core
import LLVM.ExecutionEngine qualified as EE
import LLVM.Util.Arithmetic (CallIntrinsic, arithFunction, (%<), (?))
import LLVM.Util.Arithmetic qualified as A
import LLVM.Util.File (writeCodeGenModule)
import LLVM.Util.Foreign qualified as F
import Type.Data.Num.Decimal.Literal (D4)
import Prelude hiding ((^))
import Prelude qualified as P

(^) :: (Num a) => a -> Int -> a
(^) = (P.^)

mSomeFn ::
  (IsConst a, Floating a, IsFloating a, CallIntrinsic a, CmpRet a) =>
  CodeGenModule (Function (a -> IO a))
mSomeFn = do
  foo <-
    createFunction InternalLinkage $ arithFunction $ \x y ->
      exp (sin x) + y
  createFunction ExternalLinkage $ arithFunction $ \x -> do
    y <- A.set $ x ^ 3
    sqrt (x ^ 2 - 5 * x + 6) + A.toArithFunction foo x x + y + log y

mFib :: CodeGenModule (Function (Int32 -> IO Int32))
mFib = A.recursiveFunction $ \rfib n -> n %< 2 ? (1, rfib (n - 1) + rfib (n - 2))

type V = Vector D4 Float

mVFun :: CodeGenModule (Function (Ptr V -> Ptr V -> IO ()))
mVFun = do
  fn <- createFunction ExternalLinkage $ arithFunction $ \x ->
    log x * exp x * x - 16

  vectorToPtr fn

main :: IO ()
main = do
  -- Initialize jitter
  initializeNativeTarget

  let mSomeFn' = mSomeFn
  ioSomeFn <- EE.simpleFunction mSomeFn'
  let someFn :: Double -> Double
      someFn = EE.unsafeRemoveIO ioSomeFn

  writeCodeGenModule "Arith.bc" mSomeFn'

  print (someFn 10)
  print (someFn 2)

  writeCodeGenModule "ArithFib.bc" mFib

  fib <- EE.simpleFunction mFib
  fib 22 >>= print

  writeCodeGenModule "VArith.bc" mVFun

  ioVFun <- EE.simpleFunction mVFun
  let v = consVector 1 2 3 4

  r <- vectorPtrWrap ioVFun v
  print r

vectorToPtr ::
  Function (V -> IO V) -> CodeGenModule (Function (Ptr V -> Ptr V -> IO ()))
vectorToPtr f =
  createFunction ExternalLinkage $ \px py -> do
    x <- load px
    y <- call f x
    store y py
    ret ()

vectorPtrWrap :: (Ptr V -> Ptr V -> IO ()) -> V -> IO V
vectorPtrWrap f v =
  F.with v $ \aPtr ->
    F.alloca $ \bPtr -> do
      f aPtr bPtr
      EE.peek bPtr
