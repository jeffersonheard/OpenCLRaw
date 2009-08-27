{-# LANGUAGE ForeignFunctionInterface #-}
{-| 
    Corresponds to section 5.1, Command Queues of the OpenCL 1.0 specifications.
-}
module System.OpenCL.Raw.V10.CommandQueue 
    (clCreateCommandQueue
    ,clRetainCommandQueue
    ,clGetCommandQueueInfo
    ,clSetCommandQueueProperty)
where

import System.OpenCL.Raw.V10.Types
import System.OpenCL.Raw.V10.Errors
import System.OpenCL.Raw.V10.Utils
import Foreign
import Control.Applicative
import Data.Bits
import Data.Maybe


foreign import ccall "clCreateCommandQueue" raw_clCreateCommandQueue :: Context -> DeviceID -> CLbitfield -> Ptr CLint -> IO CommandQueue
clCreateCommandQueue :: Context -> DeviceID -> CommandQueueProperties -> IO (Either ErrorCode CommandQueue)
clCreateCommandQueue ctx devid (CommandQueueProperties properties) = 
    wrapErrorEither $ raw_clCreateCommandQueue ctx devid properties 

foreign import ccall "clRetainCommandQueue" raw_clRetainCommandQueue :: CommandQueue -> IO CLint
clRetainCommandQueue :: CommandQueue -> IO (Maybe ErrorCode)
clRetainCommandQueue queue = wrapError (raw_clRetainCommandQueue queue)

foreign import ccall "clReleaseCommandQueue" raw_clReleaseCommandQueue :: CommandQueue -> IO CLint
clReleaseCommandQueue :: CommandQueue -> IO (Maybe ErrorCode)
clReleaseCommandQueue queue = wrapError (raw_clReleaseCommandQueue queue)

foreign import ccall "clGetCommandQueueInfo" raw_clGetCommandQueueInfo :: CommandQueue -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
clGetCommandQueueInfo :: CommandQueue -> CommandQueueInfo -> CLsizei -> IO (Either ErrorCode (ForeignPtr (), CLsizei))
clGetCommandQueueInfo ctx (CommandQueueInfo param_name) param_size = wrapGetInfo (raw_clGetCommandQueueInfo ctx param_name) param_size

foreign import ccall "clSetCommandQueueProperty" raw_clSetCommandQueueProperty :: CommandQueue -> CLbitfield -> CLbool -> Ptr CLbitfield -> IO CLint
clSetCommandQueueProperty :: CommandQueue -> CommandQueueProperties -> Bool -> IO (Either ErrorCode CommandQueueProperties)
clSetCommandQueueProperty queue (CommandQueueProperties properties) enable = alloca $ \old_properties -> do 
    err <- ErrorCode <$> raw_clSetCommandQueueProperty queue properties (if enable then clTrue else clFalse) old_properties
    if err == clSuccess 
        then Right . CommandQueueProperties <$> peek old_properties
        else return . Left $ err
        

