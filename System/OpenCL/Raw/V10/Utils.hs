{-| OpenCL utility functions for improving FFI wrapper code. -}
module System.OpenCL.Raw.V10.Utils where

import Foreign
import Foreign.C
import System.OpenCL.Raw.V10.Errors
import System.OpenCL.Raw.V10.Types
import Control.Applicative
import Control.Monad.Cont
import Control.Exception ( throw )

wrapError :: Integral a => IO a -> IO ()
wrapError thunk = do
  err <- (ErrorCode . fromIntegral) <$> thunk
  if err == clSuccess
    then return ()
    else throw err

wrapErrorPtr :: (Storable a, Integral a) => (Ptr a -> IO b) -> IO b
wrapErrorPtr thunk = alloca $ \errorP -> do
  ret <- thunk errorP
  err <- (ErrorCode . fromIntegral) <$> peek errorP
  if err == clSuccess
    then return ret
    else throw err

fetchPtr :: (Storable a, Integral b) => (Ptr a -> IO b) -> IO a
fetchPtr thunk = alloca $ \ptr -> do
  err <- (ErrorCode . fromIntegral) <$> thunk ptr
  if err == clSuccess
    then peek ptr
    else throw err

wrapGetInfo :: (CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint) -> CLsizei -> IO (ForeignPtr (), CLsizei)
wrapGetInfo raw_infoFn param_size = do
  param_data <- mallocForeignPtrBytes . fromIntegral $ param_size
  valsz <- withForeignPtr param_data $ \param_dataP -> fetchPtr (raw_infoFn param_size param_dataP)
  return (param_data, valsz) 

nest :: [(r -> a) -> a] -> ([r] -> a) -> a
nest xs = runCont (sequence (map cont xs))

withCStringArray0 :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray0 strings act = nest (map withCString strings)
                                     (\rs -> withArray0 nullPtr rs act)
