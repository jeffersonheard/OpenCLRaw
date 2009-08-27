{-# LANGUAGE ForeignFunctionInterface #-}
{-| Module for querying extensions -}
module System.OpenCL.Raw.V10.Etc 
    (clGetExtensionFunctionAddress)
where

import System.OpenCL.Raw.V10.Types
import System.OpenCL.Raw.V10.Errors
import System.OpenCL.Raw.V10.Utils
import Foreign
import Foreign.C
import Control.Applicative

foreign import ccall "clGetExtensionFunctionAddress" raw_clGetExtensionFunctionAddress :: CString -> IO (Ptr ())
clGetExtensionFunctionAddress :: String -> IO (Ptr ())
clGetExtensionFunctionAddress str = withCString str raw_clGetExtensionFunctionAddress
