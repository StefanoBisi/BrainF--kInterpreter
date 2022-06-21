import Data.Char (ord, chr)
import Data.Word ( Word8 )
import Control.Monad.State ( MonadState(state), State )
import Utilities ( putInList, rollDown, rollUp )

data ProgramState = Run | WaitIn | WaitOut | End | Debug deriving (Show, Eq)

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
    | _ptr < _len = _code !! _ptr
    | otherwise = '_'
    where _ptr = instructionPointer p
          _len = length _code
          _code = code p

getExecCommand :: Execution -> Char
getExecCommand (_, p) = getCommand p

nextCommand :: Process -> Process
nextCommand p
    | _ptr < _max = p { instructionPointer = succ _ptr }
    | otherwise = p { pState = End }
    where _ptr = instructionPointer p
          _max = length (code p) - 1

previousCommand :: Process -> Process
previousCommand p
    | _ptr > 0 = p { instructionPointer = pred _ptr }
    | otherwise = p { pState = End }
    where _ptr = instructionPointer p

setProgramState :: ProgramState -> State Execution ()
setProgramState s = state $ \(vm, p) -> ((), (vm, p { pState = s}))

alterMemory :: (Word8 -> Word8) -> State Execution ()
alterMemory op = state $ \(vm, p) ->
    let _memory = memory vm
        _ptr = memoryPointer p
        _value = _memory !! _ptr
        upd_vm = vm { memory = putInList _memory _ptr (op _value) } in
    ((), (upd_vm, nextCommand p))

-- instruction +
increment :: State Execution ()
increment = alterMemory rollUp

-- instruction -
decrement :: State Execution ()
decrement = alterMemory rollDown

moveMemory :: (Int -> Int) -> State Execution ()
moveMemory op = state $ \(vm, p) ->
    ((), (vm, nextCommand p { memoryPointer = op (memoryPointer p)}))

-- instruction >
moveRight :: State Execution ()
moveRight = moveMemory rollUp

-- instruction <
moveLeft :: State Execution ()
moveLeft = moveMemory rollDown

-- instruction , (input)
insertValue :: Word8 -> State Execution ()
insertValue value = state $ \(vm, p) ->
    let _memory = memory vm
        _ptr = memoryPointer p
        upd_vm = vm { memory = putInList _memory _ptr value } in
    ((), (upd_vm, nextCommand p))

-- instruction . (output)
getValue :: State Execution Char
getValue = state $ \(vm, p) ->
    let value = chr . fromIntegral $ memory vm !! memoryPointer p in
    (value, (vm, nextCommand p))

jumpToBracket :: (Process -> Process) -> Process -> Process
jumpToBracket action p = nextStep p 1 where
    nextStep p n =
        let np = stepCount (getCommand p) n in
        if pState p == End  || np == 0 then p
        else nextStep (action p) np
    stepCount c n
        | c == '[' = n + 1
        | c == ']' = n - 1
        | otherwise = n

jumpForward :: Process -> Process
jumpForward = jumpToBracket nextCommand

jumpBackward :: Process -> Process
jumpBackward = jumpToBracket previousCommand

-- instruction [
openBlock :: State Execution ()
openBlock = state $ \(vm, p) ->
    if (memory vm !! memoryPointer p) == 0
        then ((), (vm, jumpForward p))
        else ((), (vm, nextCommand p))

-- instruction ]
closeBlock :: State Execution ()
closeBlock = state $ \(vm, p) ->
    if (memory vm !! memoryPointer p) == 0
        then ((), (vm, nextCommand p))
        else ((), (vm, jumpBackward p))

-- comments
proceed :: State Execution ()
proceed = state $ \(vm, p) ->
    ((), (vm, nextCommand p))
