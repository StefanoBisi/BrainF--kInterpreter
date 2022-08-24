module Execution (
    execProgram
) where

import Data.Word ( Word8 )
import Data.Char (chr, ord)
import Control.Monad.State ( MonadState(state), State, runState )
import Utilities ( putInList, rollDown, rollUp )
import VirtualMachine ( VirtualMachine(..), IOMode(..) )
import Process
    ( Process(..), ProcessState(..), getCommand, nextCommand, jumpForward, jumpBackward )

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
-- If the input buffer is empty goes in WaitIn state, otherwise pops the heade value of the buffer and puts it in memory
readInputBuffer :: State Execution ()
readInputBuffer = state $ \(vm, p) -> case length (inputBuffer p) of
    0 -> ((), (vm, p { pState = WaitIn}))
    _ -> let _memory = memory vm
             _ptr = memoryPointer p
             _value:_newInBuffer = inputBuffer p
             upd_vm = vm { memory = putInList _memory _ptr _value }
             upd_p = nextCommand (p { inputBuffer = _newInBuffer }) in
             ((), (upd_vm, upd_p))

-- Appends a value to the input buffer, than if it is filled, goes in Run state
appendInputBuffer :: Word8 -> State Execution ()
appendInputBuffer value = state $ \(vm, p) ->
    let _newInBuffer = inputBuffer p ++ [value]
        _bufferLen = length _newInBuffer
        _ioMode = ioMode vm
        _newState = case _ioMode of
            SingleChar -> Run
            Line -> if value == 10 then Run else WaitIn -- 10 = \n
            Buffer l -> if l >= _bufferLen then Run else WaitIn in
                ((), (vm, nextCommand (p { pState = _newState, inputBuffer = _newInBuffer })))

-- instruction . (output)
-- Appends a value to the output buffer, than if it is filled, goes in WaitOut state
writeOutputBuffer :: State Execution ()
writeOutputBuffer = state $ \(vm, p) -> let
    _value = memory vm !! memoryPointer p
    _newOutBuffer = outputBuffer p ++ [_value]
    _bufferLen = length _newOutBuffer
    _ioMode = ioMode vm
    _flush = case _ioMode of
        SingleChar -> _bufferLen > 0
        Line -> _value == 10 -- 10 = \n
        Buffer l -> l >= _bufferLen in
            if _flush
            then ((), (vm, p { pState = WaitOut, outputBuffer = _newOutBuffer}))
            else ((), (vm, nextCommand (p { outputBuffer = _newOutBuffer})))

-- Clears and returns the output buffer, then goes in Run state
flushOutputBuffer :: State Execution String
flushOutputBuffer = state $ \(vm, p) ->
    let str = map (chr . fromIntegral) (outputBuffer p) in
    (str, (vm, nextCommand (p { pState = Run, outputBuffer = [] })))

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
    ',' -> readInputBuffer
    '.' -> writeOutputBuffer
    '[' -> openBlock
    ']' -> closeBlock
    _ -> proceed

getExecState :: State Execution ProcessState
getExecState = state $ \(vm, p) -> (pState p, (vm, p))

getExecCommand :: State Execution Char
getExecCommand = state $ \(vm, p) -> (getCommand p, (vm, p))

execId :: State Execution ()
execId = state $ \(vm, p) -> ((), (vm, p))

runCycle :: State Execution ()
runCycle = do
    state <- getExecState
    case state of
        Run -> do
            cmd <- getExecCommand
            getOperation cmd
            runCycle
        _ -> return ()

ioCycle :: Execution -> IO()
ioCycle execution = do
    let state = pState $ snd execution
    case state of
        Run -> do
            let (_, _newExecution) = runState runCycle execution
            ioCycle _newExecution
        WaitIn -> do
            val <- getChar
            let w8Val = fromIntegral . ord $ val
            let (_, _newExecution) = runState (appendInputBuffer w8Val) execution
            ioCycle _newExecution
        WaitOut -> do
            let (_, _newExecution) = runState flushOutputBuffer execution
            let str = map (chr . fromIntegral) (outputBuffer $ snd _newExecution)
            putStr str
            ioCycle _newExecution
        End -> return ()

execProgram :: VirtualMachine -> Process -> IO()
execProgram vm p = ioCycle (vm, p)