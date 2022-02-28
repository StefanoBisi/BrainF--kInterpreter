import Data.Char (ord, chr) -- Per la gestione I/O5
import Data.Word ( Word8 )
import Control.Monad.State ( MonadState(state), State )
import Utilities ( putInList, rollDown, rollUp )

data ProgramState = Run | WaitIn | WaitOut | End | Debug deriving (Show)

data OutputMode = SingleChar | Line | Buffer Int

data Process = Process {
    code :: String,
    instructionPointer :: Int,
    memoryPointer :: Int,
    pState :: ProgramState,
    outputBuffer :: String
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
    pState = Run,
    outputBuffer = []
}

getCommand :: Process -> Char
getCommand p
    | _ptr < _len = code p !! _ptr
    | otherwise = '_'
    where _ptr = instructionPointer p
          _len = length $ code p

getExecCommand :: Execution -> Char
getExecCommand (_, p)
    | _ptr < _len = code p !! _ptr
    | otherwise = '_'
    where _ptr = instructionPointer p
          _len = length $ code p

nextCommand :: Process -> Process
nextCommand p
    | _ptr < _len = p { instructionPointer = succ (instructionPointer p) }
    | otherwise = p { pState = End }
    where _ptr = instructionPointer p
          _len = length $ code p

setProgramState :: ProgramState -> State Execution ()
setProgramState s = state $ \(vm, p) -> ((), (vm, p { pState = s}))

alterMemory :: (Word8 -> Word8) -> State Execution ()
alterMemory op = state $ \(vm, p) -> 
    let _memory = memory vm
        _ptr = memoryPointer p
        _value = _memory !! _ptr
        upd_vm = vm { memory = putInList _memory _ptr (op _value) } in
    ((), (upd_vm, nextCommand p))

increment :: State Execution ()
increment = alterMemory rollUp

decrement :: State Execution ()
decrement = alterMemory rollDown

moveMemory :: (Int -> Int) -> State Execution ()
moveMemory op = state $ \(vm, p) ->
    ((), (vm, nextCommand p { memoryPointer = op (memoryPointer p)}))

moveRight :: State Execution ()
moveRight = moveMemory rollUp

moveLeft :: State Execution ()
moveLeft = moveMemory rollDown

insertValue :: Word8 -> State Execution ()
insertValue value = state $ \(vm, p) -> 
    let _memory = memory vm
        _ptr = memoryPointer p
        upd_vm = vm { memory = putInList _memory _ptr value } in
    ((), (upd_vm, nextCommand p))

getValue :: State Execution Char
getValue = state $ \(vm, p) ->
    let value = chr . fromIntegral $ memory vm !! memoryPointer p in
    (value, (vm, nextCommand p))

proceed :: State Execution ()
proceed = state $ \(vm, p) ->
    ((), (vm, nextCommand p))