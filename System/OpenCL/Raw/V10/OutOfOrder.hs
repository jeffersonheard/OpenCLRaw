{-# LANGUAGE ForeignFunctionInterface #-}
{-| Conforms to section 5.8 of the OpenCL 1.0 specification -}
module System.OpenCL.Raw.V10.OutOfOrder
    (clEnqueueMarker
    ,clEnqueueWaitForEvents
    ,clEnqueueBarrier)
where 

import System.OpenCL.Raw.V10.Types
import System.OpenCL.Raw.V10.Errors
import System.OpenCL.Raw.V10.Utils
import Foreign
import Control.Applicative
import Data.Maybe

foreign import ccall "clEnqueueMarker" raw_clEnqueueMarker :: CommandQueue -> Ptr Event -> IO CLint 
clEnqueueMarker :: CommandQueue -> IO (Either ErrorCode Event)
clEnqueueMarker queue = alloca $ \eventP -> do
    err <- wrapError $ raw_clEnqueueMarker queue eventP
    if err == Nothing 
        then Right <$> peek eventP
        else return $ Left . fromJust $  err
    
foreign import ccall "clEnqueueWaitForEvents" raw_clEnqueueWaitForEvents :: CommandQueue -> CLuint -> Ptr Event -> IO CLint
clEnqueueWaitForEvents :: CommandQueue -> [Event] -> IO (Maybe ErrorCode)
clEnqueueWaitForEvents queue events = 
    allocaArray num_events $ \eventsP -> do
        pokeArray eventsP events
        wrapError $ raw_clEnqueueWaitForEvents queue (fromIntegral num_events) eventsP 
    where num_events = length events

foreign import ccall "clEnqueueBarrier" raw_clEnqueueBarrier :: CommandQueue -> IO CLint 
clEnqueueBarrier :: CommandQueue -> IO (Maybe ErrorCode) 
clEnqueueBarrier queue = wrapError $ raw_clEnqueueBarrier queue


