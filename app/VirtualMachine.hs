module VirtualMachine (
    IOMode (..),
    VirtualMachine (..),
    installVM
) where

import Data.Word ( Word8 )

-- Determines how the I/O buffers are loaded/flushed:
-- byte by byte, after each line, or after the buffer has reached a predetermined size.
data IOMode = SingleChar | Line | Buffer Int

-- An execution environment for a BF program.
-- It consists of memory and I/O mode
data VirtualMachine = VirtualMachine {
    memorySize :: Int,
    memory :: [Word8],
    ioMode :: IOMode
}

-- Initializes a virtual machine
installVM :: Int -> IOMode -> VirtualMachine
installVM _memorySize _ioMode = VirtualMachine {
    memorySize = _memorySize,
    memory = replicate _memorySize 0,
    ioMode = _ioMode
}