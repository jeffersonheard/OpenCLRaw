{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
{-| Conforms to section 4.2 of the OpenCL 1.0 specification -}
module System.OpenCL.Raw.V10.DeviceInfo
    (clGetDeviceIDs
    ,clGetDeviceInfo)
where

import System.OpenCL.Raw.V10.Types
import System.OpenCL.Raw.V10.Errors
import System.OpenCL.Raw.V10.Utils
import Foreign
import Control.Applicative
import Data.Bits


foreign import ccall "clGetDeviceIDs" raw_clGetDeviceIDs :: PlatformID -> CLbitfield -> CLuint -> Ptr DeviceID -> Ptr CLuint -> IO CLint
clGetDeviceIDs :: PlatformID -> DeviceType -> CLuint -> IO (Either ErrorCode [DeviceID])
clGetDeviceIDs platform (DeviceType device_type) num_entries = alloca $ \(devices::Ptr DeviceID) -> alloca $ \(num_devices::Ptr CLuint) -> do
  errcode <- ErrorCode <$> raw_clGetDeviceIDs platform device_type num_entries devices num_devices
  if errcode == clSuccess
      then Right <$> (peek num_devices >>= \num_devicesN -> peekArray (fromIntegral num_devicesN) devices)
      else return $ Left errcode
      
foreign import ccall "clGetDeviceInfo" raw_clGetDeviceInfo :: DeviceID -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
clGetDeviceInfo :: DeviceID -> DeviceInfo -> CLsizei -> IO (Either ErrorCode (ForeignPtr (), CLsizei))
clGetDeviceInfo obj (DeviceInfo param_name) param_size = wrapGetInfo (raw_clGetDeviceInfo obj param_name) param_size


