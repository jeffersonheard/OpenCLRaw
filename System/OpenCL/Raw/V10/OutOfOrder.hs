{-# LANGUAGE ForeignFunctionInterface #-}
{-| Conforms to section 5.8 of the OpenCL 1.0 specification -}
module System.OpenCL.Raw.V10.OutOfOrder
    (clEnqueueMarker
    ,clEnqueueWaitForEvents
    ,clEnqueueBarrier)
where 

import System.OpenCL.Raw.V10.Types
import System.OpenCL.Raw.V10.Utils
import Foreign

foreign import ccall "clEnqueueMarker" raw_clEnqueueMarker :: CommandQueue -> Ptr Event -> IO CLint 
clEnqueueMarker :: CommandQueue -> IO Event
clEnqueueMarker queue = fetchPtr $ raw_clEnqueueMarker queue
    
foreign import ccall "clEnqueueWaitForEvents" raw_clEnqueueWaitForEvents :: CommandQueue -> CLuint -> Ptr Event -> IO CLint
clEnqueueWaitForEvents :: CommandQueue -> [Event] -> IO ()
clEnqueueWaitForEvents queue events = 
    allocaArray num_events $ \eventsP -> do
        pokeArray eventsP events
        wrapError $ raw_clEnqueueWaitForEvents queue (fromIntegral num_events) eventsP 
    where num_events = length events

foreign import ccall "clEnqueueBarrier" raw_clEnqueueBarrier :: CommandQueue -> IO CLint 
clEnqueueBarrier :: CommandQueue -> IO () 
clEnqueueBarrier queue = wrapError $ raw_clEnqueueBarrier queue


