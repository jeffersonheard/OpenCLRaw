{-| A collection of all the error codes that OpenCL functions return -}
module System.OpenCL.Raw.V10.Errors where

import System.OpenCL.Raw.V10.Types

clSuccess :: ErrorCode
clSuccess = ErrorCode (0)

clDeviceNotFound :: ErrorCode
clDeviceNotFound = ErrorCode (-1)

clDeviceNotAvailable :: ErrorCode
clDeviceNotAvailable = ErrorCode (-2)

clCompilerNotAvailable :: ErrorCode
clCompilerNotAvailable = ErrorCode (-3)

clMemObjectAllocationFailure :: ErrorCode
clMemObjectAllocationFailure = ErrorCode (-4)

clOutOfResources :: ErrorCode
clOutOfResources = ErrorCode (-5)

clOutOfHostMemory :: ErrorCode
clOutOfHostMemory = ErrorCode (-6)

clProfilingInfoNotAvailable :: ErrorCode
clProfilingInfoNotAvailable = ErrorCode (-7)

clMemCopyOverlap :: ErrorCode
clMemCopyOverlap = ErrorCode (-8)

clImageFormatMismatch :: ErrorCode
clImageFormatMismatch = ErrorCode (-9)

clImageFormatNotSupported :: ErrorCode
clImageFormatNotSupported = ErrorCode (-10)

clBuildProgramFailure :: ErrorCode
clBuildProgramFailure = ErrorCode (-11)

clMapFailure :: ErrorCode
clMapFailure = ErrorCode (-12)

clInvalidValue :: ErrorCode
clInvalidValue = ErrorCode (-30)

clInvalidDeviceType :: ErrorCode
clInvalidDeviceType = ErrorCode (-31)

clInvalidPlatform :: ErrorCode
clInvalidPlatform = ErrorCode (-32)

clInvalidDevice :: ErrorCode
clInvalidDevice = ErrorCode (-33)

clInvalidContext :: ErrorCode
clInvalidContext = ErrorCode (-34)

clInvalidQueueProperties :: ErrorCode
clInvalidQueueProperties = ErrorCode (-35)

clInvalidCommandQueue :: ErrorCode
clInvalidCommandQueue = ErrorCode (-36)

clInvalidHostPtr :: ErrorCode
clInvalidHostPtr = ErrorCode (-37)

clInvalidMemObject :: ErrorCode
clInvalidMemObject = ErrorCode (-38)

clInvalidImageFormatDescriptor :: ErrorCode
clInvalidImageFormatDescriptor = ErrorCode (-39)

clInvalidImageSize :: ErrorCode
clInvalidImageSize = ErrorCode (-40)

clInvalidSampler :: ErrorCode
clInvalidSampler = ErrorCode (-41)

clInvalidBinary :: ErrorCode
clInvalidBinary = ErrorCode (-42)

clInvalidBuildOptions :: ErrorCode
clInvalidBuildOptions = ErrorCode (-43)

clInvalidProgram :: ErrorCode
clInvalidProgram = ErrorCode (-44)

clInvalidProgramExecutable :: ErrorCode
clInvalidProgramExecutable = ErrorCode (-45)

clInvalidKernelName :: ErrorCode
clInvalidKernelName = ErrorCode (-46)

clInvalidKernelDefinition :: ErrorCode
clInvalidKernelDefinition = ErrorCode (-47)

clInvalidKernel :: ErrorCode
clInvalidKernel = ErrorCode (-48)

clInvalidArgIndex :: ErrorCode
clInvalidArgIndex = ErrorCode (-49)

clInvalidArgValue :: ErrorCode
clInvalidArgValue = ErrorCode (-50)

clInvalidArgSize :: ErrorCode
clInvalidArgSize = ErrorCode (-51)

clInvalidKernelArgs :: ErrorCode
clInvalidKernelArgs = ErrorCode (-52)

clInvalidWorkDimension :: ErrorCode
clInvalidWorkDimension = ErrorCode (-53)

clInvalidWorkGroupSize :: ErrorCode
clInvalidWorkGroupSize = ErrorCode (-54)
clInvalidWorkItemSize :: ErrorCode

clInvalidWorkItemSize = ErrorCode (-55)
clInvalidGlobalOffset :: ErrorCode

clInvalidGlobalOffset = ErrorCode (-56)
clInvalidEventWaitList :: ErrorCode

clInvalidEventWaitList = ErrorCode (-57)
clInvalidEvent :: ErrorCode

clInvalidEvent = ErrorCode (-58)
clInvalidOperation :: ErrorCode

clInvalidOperation = ErrorCode (-59)
clInvalidGLObject :: ErrorCode

clInvalidGLObject = ErrorCode (-60)
clInvalidBufferSize :: ErrorCode

clInvalidBufferSize = ErrorCode (-61)
clInvalidMipLevel = ErrorCode (-62)


