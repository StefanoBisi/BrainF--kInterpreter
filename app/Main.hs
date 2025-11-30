import VirtualMachine(installVM, IOMode (SingleChar))
import Process(loadProgram)
import Execution(execProgram)

import System.Environment(getArgs)
import System.IO (readFile)

defaultIOMode = SingleChar
defaultMemorySize = 30000

main = do
    args <- getArgs
    code <- readFile $ head args
    let process = loadProgram code
    let virtualMachine = installVM defaultMemorySize defaultIOMode
    execProgram virtualMachine process