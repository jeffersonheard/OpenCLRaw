{-# LANGUAGE ForeignFunctionInterface #-}
{-| Conforms to section 5.10 of the OpenCL 1.0 specification -}
module System.OpenCL.Raw.V10.FlushFinish 
    (clFlush
    ,clFinish)
where

import System.OpenCL.Raw.V10.Types
import System.OpenCL.Raw.V10.Utils

foreign import ccall "clFlush" raw_clFlush :: CommandQueue -> IO CLint
clFlush :: CommandQueue -> IO ()
clFlush queue = wrapError $ raw_clFlush queue

foreign import ccall "clFinish" raw_clFinish :: CommandQueue -> IO CLint
clFinish :: CommandQueue -> IO ()
clFinish queue = wrapError $ raw_clFinish queue

