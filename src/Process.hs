module Process (
    Process (..),
    ProcessState (..),
    loadProgram,
    getCommand,
    nextCommand,
    jumpForward,
    jumpBackward
) where

import Data.Word ( Word8 )

data ProcessState = Run | WaitIn | WaitOut | End deriving Eq

data Process = Process {
    code :: String,
    instructionPointer :: Int,
    memoryPointer :: Int,
    pState :: ProcessState,
    outputBuffer :: [Word8],
    inputBuffer :: [Word8]
}

loadProgram :: String -> Process
loadProgram _code = Process {
    code = _code,
    instructionPointer = 0,
    memoryPointer = 0,
    pState = Run,
    outputBuffer = [],
    inputBuffer = []
}

getCommand :: Process -> Char
getCommand p
    | _ptr < _len = _code !! _ptr
    | otherwise = '_'
    where _ptr = instructionPointer p
          _len = length _code
          _code = code p

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