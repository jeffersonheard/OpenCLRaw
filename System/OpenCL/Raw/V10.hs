{-|
    The OpenCL system for open heterogenous data parallel supercomputing.
    
    Wrapper to the FFI written and maintained by Jeff Heard of the Renaissance Computing Institute <mailto:jeff@renci.org>
    
    From the introduction: 
    
    OpenCL (Open Computing Language) is an open royalty-free standard for general purpose 
    parallel programming across CPUs, GPUs and other processors, giving software developers 
    portable and efficient access to the power of these heterogeneous processing platforms.   

    OpenCL supports a wide range of applications, ranging from embedded and consumer software 
    to HPC solutions, through a low-level, high-performance, portable abstraction.  By creating an 
    efficient, close-to-the-metal programming interface, OpenCL will form the foundation layer of a 
    parallel computing ecosystem of platform-independent tools, middleware and applications.  
    OpenCL is particularly suited to play an increasingly significant role in emerging interactive 
    graphics applications that combine general parallel compute algorithms with graphics rendering 
    pipelines. 

    OpenCL consists of an API for coordinating parallel computation across 
    heterogeneous processors; and a cross-platform programming language with a well- 
    specified computation environment.  The OpenCL standard: 

    * Supports both data- and task-based parallel programming models 
    
    * Utilizes a subset of ISO C99 with extensions for parallelism 
    
    * Defines consistent numerical requirements based on IEEE 754 
    
    * Defines a configuration profile for handheld and embedded devices 
    
    * Efficiently interoperates with OpenGL, OpenGL ES and other graphics APIs
    
    _General Notes on the differences between Haskell and the OpenCL-C implementation_
    
    * Side-effectful procedures capable of returning an error code only return a Maybe ErrorCode, with Nothing returned upon success
    
    * Procedures which follow the pattern of returning a pointer to an object and taking a final parameter as an error code instead
      return Either ErrorCode @ObjectType@ 
      
    * Procedures which prefix with clGetInfo* merely take the object, parameter name, and parameter size to allocate.  The allocation
      handled by OpenCLRaw and returned as a Haskell-managed @ForeignPtr ()@
      
    * Enumerations and constants are replaced by newtypes for the sake of type-safety.
-}
module System.OpenCL.Raw.V10 
    (module System.OpenCL.Raw.V10.CommandQueue
    ,module System.OpenCL.Raw.V10.Context
    ,module System.OpenCL.Raw.V10.DeviceInfo
    ,module System.OpenCL.Raw.V10.Errors
    ,module System.OpenCL.Raw.V10.Etc
    ,module System.OpenCL.Raw.V10.EventObject
    ,module System.OpenCL.Raw.V10.FlushFinish
    ,module System.OpenCL.Raw.V10.Kernel
    ,module System.OpenCL.Raw.V10.MemoryObject
    ,module System.OpenCL.Raw.V10.OutOfOrder
    ,module System.OpenCL.Raw.V10.PlatformInfo
    ,module System.OpenCL.Raw.V10.ProgramObject
    ,module System.OpenCL.Raw.V10.Sampler
    ,module System.OpenCL.Raw.V10.Types)    
where

import System.OpenCL.Raw.V10.CommandQueue
import System.OpenCL.Raw.V10.Context
import System.OpenCL.Raw.V10.DeviceInfo
import System.OpenCL.Raw.V10.Errors
import System.OpenCL.Raw.V10.Etc
import System.OpenCL.Raw.V10.EventObject
import System.OpenCL.Raw.V10.FlushFinish
import System.OpenCL.Raw.V10.Kernel
import System.OpenCL.Raw.V10.MemoryObject
import System.OpenCL.Raw.V10.OutOfOrder
import System.OpenCL.Raw.V10.PlatformInfo
import System.OpenCL.Raw.V10.ProgramObject
import System.OpenCL.Raw.V10.Sampler
import System.OpenCL.Raw.V10.Types
