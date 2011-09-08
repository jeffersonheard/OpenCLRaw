{-# LANGUAGE ForeignFunctionInterface #-}
{-| Conforms to section 5.3 of the OpenCL 1.0 specification -}
module System.OpenCL.Raw.V10.Sampler
    (clCreateSampler
    ,clRetainSampler
    ,clReleaseSampler
    ,clGetSamplerInfo)
where

import System.OpenCL.Raw.V10.Types
import System.OpenCL.Raw.V10.Utils
import Foreign



foreign import ccall "clCreateSampler" raw_clCreateSampler :: Context -> CLbool -> CLuint -> CLuint -> Ptr CLint -> IO Sampler
clCreateSampler :: Context -> Bool -> AddressingMode -> FilterMode -> IO Sampler
clCreateSampler ctx normalized_coords (AddressingMode addressing_mode) (FilterMode filter_mode) = 
    wrapErrorPtr $ raw_clCreateSampler ctx (if normalized_coords then clTrue else clFalse) addressing_mode filter_mode

foreign import ccall "clRetainSampler" raw_clRetainSampler :: Sampler -> IO CLint
clRetainSampler :: Sampler -> IO () 
clRetainSampler sampler = wrapError $ raw_clRetainSampler sampler

foreign import ccall "clReleaseSampler" raw_clReleaseSampler :: Sampler -> IO CLint
clReleaseSampler :: Sampler -> IO () 
clReleaseSampler sampler = wrapError $ raw_clReleaseSampler sampler

foreign import ccall "clGetSamplerInfo" raw_clGetSamplerInfo :: Sampler -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
clGetSamplerInfo :: Sampler -> SamplerInfo -> CLsizei -> Ptr () -> IO CLsizei
clGetSamplerInfo mem (SamplerInfo param_name) param_value_size param_value = alloca $ \param_value_size_ret -> do
    wrapError $ raw_clGetSamplerInfo mem param_name param_value_size param_value param_value_size_ret
    peek param_value_size_ret
