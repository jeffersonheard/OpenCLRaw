{-| OpenCL utility functions for improving FFI wrapper code. -}
module System.OpenCL.Raw.V10.Utils where

import Foreign
import Foreign.C
import System.OpenCL.Raw.V10.Errors
import System.OpenCL.Raw.V10.Types
import Control.Applicative
import Data.Maybe
import Control.Monad.Cont

wrapError :: IO CLint -> IO (Maybe ErrorCode)
wrapError thunk = thunk >>= \errcode -> if ErrorCode errcode == clSuccess then return Nothing else return . Just . ErrorCode $ errcode

wrapErrorEither :: (Ptr CLint -> IO a) -> IO (Either ErrorCode a)
wrapErrorEither thunk = alloca $ \errorP -> do
    ret <- thunk errorP
    err <- ErrorCode <$> peek errorP
    if err == clSuccess
        then return . Right $ ret
        else return . Left $ err 
                
wrapGetInfo :: (CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint) -> CLsizei -> IO (Either ErrorCode (ForeignPtr (), CLsizei))
wrapGetInfo raw_infoFn param_size = alloca $ \value_size_ret -> do
    param_data <- (mallocForeignPtrBytes . fromIntegral $ param_size) :: IO (ForeignPtr ())
    ret <- wrapError $ withForeignPtr param_data $ \param_dataP -> raw_infoFn param_size param_dataP value_size_ret
    if ret == Just clSuccess
        then peek value_size_ret >>= \valsz -> return . Right $ (param_data,valsz)
        else return . Left $ fromJust ret        



nest :: [(r -> a) -> a] -> ([r] -> a) -> a
nest xs = runCont (sequence (map Cont xs))

withCStringArray0 :: [String] -> (Ptr CString -> IO a) -> IO a
withCStringArray0 strings act = nest (map withCString strings)
                                     (\rs -> withArray0 nullPtr rs act)
