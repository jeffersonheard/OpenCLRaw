{-# LANGUAGE ForeignFunctionInterface #-}
{-|
    Conforms to section 4.3 of the OpenCL 1.0 specification
-}
module System.OpenCL.Raw.V10.Context 
    (clCreateContext
    ,ContextCallback
    ,clCreateContextFromType
    ,clRetainContext
    ,clReleaseContext
    ,clGetContextInfo
    )
where

import System.OpenCL.Raw.V10.Types
import System.OpenCL.Raw.V10.Errors
import System.OpenCL.Raw.V10.Utils
import Foreign
import Foreign.C
import Control.Applicative
import Data.Bits
import Data.Maybe


type ContextCallback = (CString -> Ptr () -> CLsizei -> Ptr () -> IO ())
foreign import ccall "clCreateContext" raw_clCreateContext :: Ptr (Ptr CLint) -> CLuint -> Ptr DeviceID -> FunPtr ContextCallback -> Ptr () -> Ptr CLint -> IO Context
foreign import ccall "wrapper" wrapCreateContextCallback :: ContextCallback -> IO (FunPtr ContextCallback)

clCreateContext :: [ContextProperties] -> [DeviceID] -> ContextCallback -> Ptr () -> IO (Either ErrorCode Context)    
clCreateContext properties devices pfn_notify user_dat =
    allocaArray (propertiesN+1) $ \propertiesP -> allocaArray devicesN $ \devicesP -> do
        pokeArray0 nullPtr propertiesP properties
        pokeArray devicesP devices
        fptr <- wrapCreateContextCallback pfn_notify
        wrapErrorEither $ raw_clCreateContext propertiesP (fromIntegral devicesN) devicesP fptr user_dat             
    where propertiesN = length properties
          devicesN = length devices
          
    
foreign import ccall "clCreateContextFromType" raw_clCreateContextFromType :: Ptr ContextProperties -> CLbitfield -> FunPtr ContextCallback -> Ptr a -> Ptr CLint -> IO Context

clCreateContextFromType :: [ContextProperties] -> DeviceType -> ContextCallback -> Ptr () -> IO (Either ErrorCode Context)
clCreateContextFromType properties (DeviceType device_type) pfn_notify user_data = allocaArray (propertiesN+1) $ \propertiesP -> do
    pokeArray0 nullPtr propertiesP properties
    fptr <- wrapCreateContextCallback pfn_notify
    wrapErrorEither $ raw_clCreateContextFromType propertiesP device_type fptr user_data
    where propertiesN = length properties 
    
foreign import ccall "clRetainContext" raw_clRetainContext :: Context -> IO CLint
clRetainContext :: Context -> IO (Maybe ErrorCode)
clRetainContext ctx = wrapError (raw_clRetainContext ctx)

foreign import ccall "clReleaseContext" raw_clReleaseContext :: Context -> IO CLint
clReleaseContext :: Context -> IO (Maybe ErrorCode)
clReleaseContext ctx = wrapError (raw_clReleaseContext ctx)

foreign import ccall "clGetContextInfo" raw_clGetContextInfo :: Context -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
clGetContextInfo :: Context -> ContextInfo -> CLsizei -> IO (Either ErrorCode (ForeignPtr (), CLsizei))
clGetContextInfo ctx (ContextInfo param_name) param_size = wrapGetInfo (raw_clGetContextInfo ctx param_name) param_size



