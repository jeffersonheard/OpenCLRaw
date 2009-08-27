{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
{-| Conforms to section 4.1 of the OpenCL 1.0 specification -}
module System.OpenCL.Raw.V10.PlatformInfo (
    clGetPlatformIDs
  , clGetPlatformInfo
  ) where

import System.OpenCL.Raw.V10.Types
import System.OpenCL.Raw.V10.Errors
import System.OpenCL.Raw.V10.Utils
import Foreign
import Foreign.C
import Control.Applicative
import Data.Maybe


foreign import ccall "clGetPlatformIDs" raw_clGetPlatformIDs :: CLuint -> Ptr PlatformID -> Ptr CLuint -> IO CLint


clGetPlatformIDs :: CLuint -> IO (Either ErrorCode [PlatformID])
clGetPlatformIDs num_entries = alloca $ \(platforms::Ptr PlatformID) -> alloca $ \(num_platforms::Ptr CLuint) -> do
  errcode <- ErrorCode <$> raw_clGetPlatformIDs (fromIntegral num_entries) platforms num_platforms
  if errcode == clSuccess
      then Right <$> (peek num_platforms >>= \num_platformsN -> peekArray (fromIntegral num_platformsN) platforms)
      else return $ Left errcode
      
      
foreign import ccall "clGetPlatformInfo" raw_clGetPlatformInfo :: PlatformID -> CLuint -> CSize -> Ptr () -> Ptr CSize -> IO CLint 

clGetPlatformInfo :: PlatformID -> PlatformInfo -> CLsizei -> Ptr () -> IO (Either ErrorCode CLsizei)
clGetPlatformInfo mem (PlatformInfo param_name) param_value_size param_value = alloca $ \param_value_size_ret -> do
    err <- wrapError $ raw_clGetPlatformInfo mem param_name param_value_size param_value param_value_size_ret
    if err == Nothing
        then peek param_value_size_ret >>= return . Right 
        else return . Left . fromJust $ err
