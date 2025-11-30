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

findMatchingBracket :: Process -> Process
findMatchingBracket p = case getCommand p of
    '[' -> iteration (1, 0) nextCommand p
    ']' -> iteration (0, 1) previousCommand p
    _ -> p
    where
        iteration :: (Int, Int) -> (Process -> Process) -> Process -> Process
        iteration count action p = let
            newP = action p
            currCmd = getCommand newP
            newCount = stepCount currCmd count in
                if pState p == End  || uncurry (==) newCount then p
                else iteration newCount action newP
        stepCount :: Char -> (Int, Int) -> (Int, Int)
        stepCount char count
            | char == '[' = (fst count + 1, snd count)
            | char == ']' = (fst count, snd count + 1)
            | otherwise = count

jumpForward :: Process -> Process
jumpForward = findMatchingBracket
jumpBackward :: Process -> Process
jumpBackward = findMatchingBracket