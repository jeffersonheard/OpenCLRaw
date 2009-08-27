{-# LANGUAGE ForeignFunctionInterface #-}
{-| Conforms to section 5.7 of the OpenCL 1.0 specification -}
module System.OpenCL.Raw.V10.EventObject 
    (clWaitForEvents
    ,clGetEventInfo
    ,clRetainEvent
    ,clReleaseEvent
    ,clGetEventProfilingInfo)
where 

import System.OpenCL.Raw.V10.Types
import System.OpenCL.Raw.V10.Errors
import System.OpenCL.Raw.V10.Utils
import System.OpenCL.Raw.V10.Utils
import Foreign
import Control.Applicative


foreign import ccall "clWaitForEvents" raw_clWaitForEvents :: CLuint -> Ptr Event -> IO CLint
clWaitForEvents :: [Event] -> IO (Maybe ErrorCode)
clWaitForEvents evts = allocaArray nEvents $ \eventP -> pokeArray eventP evts >> (wrapError $ raw_clWaitForEvents (fromIntegral nEvents) eventP)
    where nEvents = length evts
                            
foreign import ccall "clGetEventInfo" raw_clGetEventInfo :: Event -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
clGetEventInfo :: Event -> EventInfo -> CLsizei -> IO (Either ErrorCode (ForeignPtr (), CLsizei))
clGetEventInfo obj (EventInfo param_name) param_size = wrapGetInfo (raw_clGetEventInfo obj param_name) param_size

foreign import ccall "clRetainEvent" raw_clRetainEvent :: Event -> IO CLint 
clRetainEvent :: Event -> IO (Maybe ErrorCode)
clRetainEvent evt = wrapError $ raw_clRetainEvent evt

foreign import ccall "clReleaseEvent" raw_clReleaseEvent :: Event -> IO CLint 
clReleaseEvent :: Event -> IO (Maybe ErrorCode)
clReleaseEvent evt = wrapError $ raw_clReleaseEvent evt 

foreign import ccall "clGetEventProfilingInfo" raw_clGetEventProfilingInfo :: Event -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
clGetEventProfilingInfo :: Event -> ProfilingInfo -> CLsizei -> IO (Either ErrorCode (ForeignPtr (), CLsizei))
clGetEventProfilingInfo obj (ProfilingInfo param_name) param_size = wrapGetInfo (raw_clGetEventProfilingInfo obj param_name) param_size                                
