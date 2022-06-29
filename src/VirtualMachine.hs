module VirtualMachine (
    OutputMode (..),
    VirtualMachine (..),
    installVM
) where

import Data.Word ( Word8 )

data OutputMode = SingleChar | Line | Buffer Int

data VirtualMachine = VirtualMachine {
    memorySize :: Int,
    memory :: [Word8],
    outputMode :: OutputMode
}

installVM :: Int -> OutputMode -> VirtualMachine
installVM _memorySize _outputMode = VirtualMachine {
    memorySize = _memorySize,
    memory = replicate _memorySize 0,
    outputMode = _outputMode
}