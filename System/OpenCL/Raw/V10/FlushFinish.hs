{-# LANGUAGE ForeignFunctionInterface #-}
{-| Conforms to section 5.10 of the OpenCL 1.0 specification -}
module System.OpenCL.Raw.V10.FlushFinish 
    (clFlush
    ,clFinish)
where

import System.OpenCL.Raw.V10.Types
import System.OpenCL.Raw.V10.Errors
import System.OpenCL.Raw.V10.Utils
import Foreign
import Control.Applicative

foreign import ccall "clFlush" raw_clFlush :: CommandQueue -> IO CLint
clFlush :: CommandQueue -> IO (Maybe ErrorCode)
clFlush queue = wrapError $ raw_clFlush queue

foreign import ccall "clFinish" raw_clFinish :: CommandQueue -> IO CLint
clFinish :: CommandQueue -> IO (Maybe ErrorCode)
clFinish queue = wrapError $ raw_clFinish queue

