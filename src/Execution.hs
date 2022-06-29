import Data.Word ( Word8 )
import Data.Char (chr)
import Control.Monad.State ( MonadState(state), State )
import Utilities ( putInList, rollDown, rollUp )
import VirtualMachine ( VirtualMachine(memory) )
import Process
    ( Process(memoryPointer, pState), ProcessState(..), getCommand, nextCommand, jumpForward, jumpBackward )

type Execution = (VirtualMachine, Process)

setProgramState :: ProcessState -> State Execution ()
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
requestInput :: State Execution ()
requestInput = state $ \(vm, p) ->
    ((), (vm, p { pState = WaitIn}))

insertValue :: Word8 -> State Execution ()
insertValue value = state $ \(vm, p) ->
    let _memory = memory vm
        _ptr = memoryPointer p
        upd_vm = vm { memory = putInList _memory _ptr value } in
    ((), (upd_vm, nextCommand (p { pState = Run })))

-- instruction . (output)
requestOutput :: State Execution ()
requestOutput = state $ \(vm, p) ->
    ((), (vm, p { pState = WaitOut}))

getValue :: State Execution Char
getValue = state $ \(vm, p) ->
    let value = chr . fromIntegral $ memory vm !! memoryPointer p in
    (value, (vm, nextCommand (p { pState = Run })))

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

getOperation :: Char -> State Execution ()
getOperation cmd = case cmd of
    '+' -> increment
    '-' -> decrement
    '>' -> moveRight
    '<' -> moveLeft
    ',' -> requestInput
    '.' -> requestOutput
    '[' -> openBlock
    ']' -> closeBlock
    _ -> proceed

getExecState :: State Execution ProcessState
getExecState = state $ \(vm, p) -> (pState p, (vm, p))

getExecCommand :: State Execution Char
getExecCommand = state $ \(vm, p) -> (getCommand p, (vm, p))

execId :: State Execution ()
execId = state $ \(vm, p) -> ((), (vm, p))

execProgram :: State Execution ()
execProgram = do
    state <- getExecState
    case state of
        Run -> do
            cmd <- getExecCommand
            getOperation cmd
            execProgram
        WaitIn -> undefined
        WaitOut -> undefined
        End -> execId
        Debug -> undefined
