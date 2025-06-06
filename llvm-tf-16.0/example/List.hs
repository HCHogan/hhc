{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Main (main) where

import qualified LLVM.ExecutionEngine as EE
import LLVM.Util.Loop (Phi, phis, addPhis, )
import LLVM.Core as LLVM
import qualified System.IO as IO

import Data.Word (Word32, )
import Data.Int (Int32, )
import Foreign.Marshal.Array (allocaArray, )
import qualified Foreign.Storable as St

import Foreign.StablePtr (StablePtr, newStablePtr, freeStablePtr, deRefStablePtr, )
import Foreign.Ptr (FunPtr)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, )


{-
I had to export Phi's methods in llvm-0.6.8
in order to be able to implement this function.
-}
arrayLoop ::
   (Phi a, IsType b,
    Num i, IsConst i, IsInteger i, IsFirstClass i, CmpRet i, CmpResult i ~ Bool) =>
   Value i -> Value (Ptr b) -> a ->
   (Value (Ptr b) -> a -> CodeGenFunction r a) ->
   CodeGenFunction r a
arrayLoop len ptr start loopBody = do
   top <- getCurrentBasicBlock
   loop <- newBasicBlock
   body <- newBasicBlock
   exit <- newBasicBlock

   br loop

   defineBasicBlock loop
   i <- phi [(len, top)]
   p <- phi [(ptr, top)]
   vars <- phis top start
   t <- cmp CmpNE i (valueOf 0 `asTypeOf` len)
   condBr t body exit

   defineBasicBlock body

   vars' <- loopBody p vars
   i' <- sub i (valueOf 1 `asTypeOf` len)
   p' <- getElementPtr p (valueOf 1 :: Value Word32, ())

   body' <- getCurrentBasicBlock
   addPhis body' vars vars'
   addPhiInputs i [(i', body')]
   addPhiInputs p [(p', body')]
   br loop

   defineBasicBlock exit
   return vars


mList ::
   CodeGenModule (Function
      (StablePtr (IORef [Word32]) -> Word32 -> Ptr Word32 -> IO Int32))
mList =
   createFunction ExternalLinkage $ \ ref size ptr -> do
     next <- staticNamedFunction "next" nelem
     s <- arrayLoop size ptr (valueOf 0) $ \ ptri y -> do
       flip store ptri =<< call next ref
       return y
     ret (s :: Value Int32)

renderList :: IO ()
renderList = do
   m <- createModule $ setTarget hostTriple >> mList >> getModule
   writeBitcodeToFile "List.bc" m

   fill <- EE.simpleFunction mList
   stable <- newStablePtr =<< newIORef [3,5..]
   IO.withFile "listcontent.u32" IO.WriteMode $ \h ->
     let len = 100
     in  allocaArray len $ \ ptr ->
           fill stable (fromIntegral len) (LLVM.fromPtr ptr) >>
           IO.hPutBuf h ptr (len * St.sizeOf(undefined::Int32))
   freeStablePtr stable


foreign import ccall "&nextListElement"
   nelem :: FunPtr (StablePtr (IORef [Word32]) -> IO Word32)

foreign export ccall
   nextListElement :: StablePtr (IORef [Word32]) -> IO Word32

nextListElement :: StablePtr (IORef [Word32]) -> IO Word32
nextListElement stable =
   do ioRef <- deRefStablePtr stable
      xt <- readIORef ioRef
      case xt of
         [] -> return 0
         (x:xs) -> writeIORef ioRef xs >> return x


main :: IO ()
main = do
    -- Initialize jitter
    initializeNativeTarget
    renderList
