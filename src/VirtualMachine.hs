module VirtualMachine (
    IOMode (..),
    VirtualMachine (..),
    installVM
) where

import Data.Word ( Word8 )

data IOMode = SingleChar | Line | Buffer Int

data VirtualMachine = VirtualMachine {
    memorySize :: Int,
    memory :: [Word8],
    ioMode :: IOMode
}

installVM :: Int -> IOMode -> VirtualMachine
installVM _memorySize _ioMode = VirtualMachine {
    memorySize = _memorySize,
    memory = replicate _memorySize 0,
    ioMode = _ioMode
}