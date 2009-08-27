{-# LANGUAGE ForeignFunctionInterface #-}
{-| Conforms to section 5.4 of the OpenCL 1.0 specification -}
module System.OpenCL.Raw.V10.ProgramObject 
    (clCreateProgramWithSource
    ,clCreateProgramWithBinary
    ,clRetainProgram
    ,clReleaseProgram
    ,clBuildProgram
    ,clUnloadCompiler
    ,clGetProgramInfo
    ,clGetProgramBuildInfo)
where

import Control.Monad.Cont
import System.OpenCL.Raw.V10.Types
import System.OpenCL.Raw.V10.Errors
import System.OpenCL.Raw.V10.Utils
import Foreign
import Foreign.C
import Control.Applicative
import qualified Data.ByteString as SBS
import qualified Data.ByteString.Internal as SBS

foreign import ccall "clCreateProgramWithSource" raw_clCreateProgramWithSource :: Context -> CLuint -> Ptr CString -> Ptr CLsizei -> Ptr CLint -> IO Program
clCreateProgramWithSource :: Context -> String -> IO (Either ErrorCode Program) 
clCreateProgramWithSource ctx source_code = do
    let count = length strings
        strings = lines source_code
        lengths = (fromIntegral . length) <$> strings
    withArray lengths $ (\lengthsP -> 
        withCStringArray0 strings $ (\stringsP -> 
            wrapErrorEither $ raw_clCreateProgramWithSource ctx (fromIntegral count) stringsP lengthsP))   

foreign import ccall "clCreateProgramWithBinary" raw_clCreateProgramWithBinary :: Context -> CLuint -> Ptr DeviceID -> Ptr CLsizei -> Ptr (Ptr Word8) -> Ptr CLint -> Ptr CLint -> IO Program
clCreateProgramWithBinary :: Context -> [(DeviceID,SBS.ByteString)] ->  IO (Either ErrorCode Program)
clCreateProgramWithBinary context devbin_pair = 
    allocaArray num_devices $ \lengths -> 
    allocaArray num_devices $ \binaries ->
    allocaArray num_devices $ \devices -> 
    alloca $ \binary_status ->
    alloca $ \errcode_ret -> do
        pokeArray lengths (map (fromIntegral . SBS.length) bins)
        pokeArray devices device_list
        pokeArray binaries ((unsafeForeignPtrToPtr . bsPtr) `map` bins) 
        program <- raw_clCreateProgramWithBinary context (fromIntegral num_devices) devices lengths binaries binary_status errcode_ret
        errcode <- ErrorCode <$> peek errcode_ret
        binstatus <- ErrorCode <$> peek binary_status
        if errcode == clSuccess && binstatus == clSuccess
            then return $ Right program
            else return $ Left (if errcode == clSuccess then binstatus else errcode)
    where bsPtr (SBS.PS p _ _) = p
          num_devices = length device_list
          (device_list,bins) = unzip devbin_pair
        
foreign import ccall "clRetainProgram" raw_clRetainProgram :: Program -> IO CLint
clRetainProgram :: Program -> IO (Maybe ErrorCode) 
clRetainProgram prog = wrapError $ raw_clRetainProgram prog

foreign import ccall "clReleaseProgram" raw_clReleaseProgram :: Program -> IO CLint
clReleaseProgram :: Program -> IO (Maybe ErrorCode) 
clReleaseProgram prog = wrapError $ raw_clReleaseProgram prog

type BuildProgramCallback = Program -> Ptr () -> IO ()
foreign import ccall "wrapper" wrapBuildProgramCallback :: BuildProgramCallback -> IO (FunPtr BuildProgramCallback)
foreign import ccall "clBuildProgram" raw_clBuildProgram :: Program -> CLuint -> Ptr DeviceID -> CString -> FunPtr BuildProgramCallback -> Ptr () -> IO CLint
clBuildProgram :: Program -> [DeviceID] -> String -> BuildProgramCallback -> Ptr () -> IO (Maybe ErrorCode)
clBuildProgram program devices ops pfn_notifyF user_data = 
    allocaArray num_devices $ \device_list -> 
    withCString ops $ \options -> do 
        pokeArray device_list devices
        pfn_notify <- wrapBuildProgramCallback pfn_notifyF
        wrapError $ raw_clBuildProgram program (fromIntegral num_devices) device_list options pfn_notify user_data
    where num_devices = length devices   

foreign import ccall "clUnloadCompiler" raw_clUnloadCompiler :: IO CLint
clUnloadCompiler :: IO (Maybe ErrorCode)
clUnloadCompiler = wrapError $ raw_clUnloadCompiler

foreign import ccall "clGetProgamInfo" raw_clGetProgramInfo :: Program -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
clGetProgramInfo :: Program -> ProgramInfo -> CLsizei -> IO (Either ErrorCode (ForeignPtr (), CLsizei))
clGetProgramInfo program (ProgramInfo param_name) param_value_size = wrapGetInfo (raw_clGetProgramInfo program param_name) param_value_size

foreign import ccall "clGetProgramBuildInfo"  raw_clGetProgramBuildInfo :: Program -> CLuint -> CLsizei -> Ptr () -> Ptr CLsizei -> IO CLint
clGetProgramBuildInfo :: Program -> ProgramBuildInfo -> CLsizei -> IO (Either ErrorCode (ForeignPtr (), CLsizei))
clGetProgramBuildInfo program (ProgramBuildInfo param_name) param_value_size = wrapGetInfo (raw_clGetProgramInfo program param_name) param_value_size

