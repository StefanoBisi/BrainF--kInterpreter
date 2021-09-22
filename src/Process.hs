import Data.Char (ord, chr) -- Per la gestione I/O5
import Data.Word ( Word8 )
import Control.Monad.State
import Utilities (putInList)

data ProgramState = Run | WaitIn | WaitOut | End | Debug deriving (Show)

data OutputMode = SingleChar | Line | WindowsLine | Buffer Int

data Process = Process {
    code :: String,
    instructionPointer :: Int,
    memoryPointer :: Int,
    pState :: ProgramState
}

data VirtualMachine = VirtualMachine {
    memorySize :: Int,
    memory :: [Word8],
    outputMode :: OutputMode
}

type Execution = (VirtualMachine, Process)

installVM :: Int -> OutputMode -> VirtualMachine
installVM _memorySize _outputMode = VirtualMachine {
    memorySize = _memorySize,
    memory = replicate _memorySize 0,
    outputMode = _outputMode
}

loadProgram :: String -> Process
loadProgram _code = Process {
    code = _code,
    instructionPointer = 0,
    memoryPointer = 0,
    pState = Run
}

insertValue :: Word8 -> State Execution ()
insertValue value = state $ \(vm, p) -> 
    let list = memory vm
        position = memoryPointer p
        newVM = vm { memory = putInList list position value } in
    ((), (newVM, p))

getValue :: State Execution Char
getValue = state $ \(vm, p) ->
    let value = chr . fromIntegral $ memory vm !! memoryPointer p in
    (value, (vm, p))

proceed :: State Execution ()
proceed = state $ \(vm, p) ->
    ((), (vm, p { instructionPointer = succ $ instructionPointer p}))