import System.Environment
import System.IO (readFile)
import Data.Char (ord, chr) -- Per la gestione I/O5
import Data.Word

--
data ProgramState = Run | WaitIn | WaitOut | End | Debug deriving (Show)

-- Struttura dati del programma
-- code: stringa contenente il codice, non va mai cambiata
-- istPointer: puntatore alla prossima istruzione da eseguire
-- memPointer: puntatore all'attuale cella di memoria
-- memory: array contenente la memoria. Inizializzato come array infinito di zeri. (Devo verificare che vada bene)
-- stack: le posizioni nel codice delle parentesi aperte incontrate. Funziona come pila, dove la parentesi più recente è alla posizione 0
-- state: stato del programma, che indica se può proseguire o se deve attendere operazioni di I/O
-- Ogni funzione deve agire su questa struttura dati, prendendola in argomento e restituendone una versione modificata
data Program = Program {
    code :: String,
    istPointer :: Int,
    memPointer :: Int,
    memory :: [Word8],
    stack :: [Int],
    state :: ProgramState} deriving (Show)

-- Riceve in input una stringa contenente il codice e restituisce un programma con i valori inizializzati
newProgram :: String -> Program
newProgram newCode = Program {code = newCode,
                                istPointer = 0,
                                memPointer = 0,
                                memory = take 10000 $ repeat 0,
                                stack = [],
                                state = Run}

-- Se ogni carattere della stringa-codice appartiene al set di comandi BrainFuck restituisce True, altrimenti False
--validateProgram :: String -> Bool
--validateProgram = foldl (\acc x -> acc && (elem x commands)) True
    --where commands = ["+","-","<",">",".",",","[","]"]

-- Inserisce un valore ad una carte posizione dell'array-memoria. (Sicuramente c'è già qualcosa di simile, devo trovarlo)
insertInMemory :: [Word8] -> Int -> Word8 -> [Word8]
insertInMemory [] _ _ = []
insertInMemory lst pos val = (take pos lst) ++ [val] ++ (drop (succ pos) lst)

-- Incrementa di uno istPointer e modifica il valore della memoria della cella puntata
-- Il valore da aggiungere alla cella deve essere fornito
alterMemory :: (Word8 -> Word8) -> Program -> Program
alterMemory action prog = prog {
    istPointer = succ $ istPointer prog,
    memory = newMem} where
        newMem = insertInMemory mem memPtr $ action $ mem !! memPtr
        mem = memory prog
        memPtr = memPointer prog

-- Incrementa di uno istPointer e modifica il memPointer
-- Il valore da aggiungere a memPointer deve essere fornito
moveMemory :: (Int -> Int) -> Program -> Program
moveMemory action prog = prog {
    istPointer = succ $ istPointer prog,
    memPointer = action $ memPointer prog}

findClose :: String -> Int -> Int -> Int
findClose code pos count = case (code !! pos) of
    '[' -> findClose code (succ pos) (succ count)
    ']' -> if (count == 0)
        then pos
        else findClose code (succ pos) (pred count)
    otherwise -> findClose code (succ pos) count

rollUp :: Word8 -> Word8
rollUp val = case val of
    255 -> 0
    otherwise -> succ val

rollDown :: Word8 -> Word8
rollDown val = case val of
    0 -> 255
    otherwise -> pred val

-- Incrementa di uno il valore della cella puntata e fa progredire istPointer
execAdd :: Program -> Program
execAdd = alterMemory rollUp --succ

-- Decrementa di uno il valore della cella puntata e fa progredire istPointer
execSub :: Program -> Program
execSub = alterMemory rollDown --pred

-- Decrementa di uno il puntatore alla memoria e fa progredire istPointer
execRight :: Program -> Program
execRight = moveMemory succ

-- Decrementa di uno il puntatore alla memoria e fa progredire istPointer
execLeft :: Program -> Program
execLeft = moveMemory pred

-- Aggiunge alla stack il puntatore all'istruzione corrente e poi lo fa progredire
execOpen :: Program -> Program
execOpen prog = if ((memory prog) !! (memPointer prog)) == 0
    then prog { istPointer = succ $ findClose (code prog) (succ $ istPointer prog) 0 }
    else prog {
        istPointer = succ $ istPointer prog,
        stack = istPtr:st} where
            istPtr = istPointer prog
            st = stack prog

-- Rimuove il primo elemento della stack
-- Se il valore della cella di memoria puntata è 0, fa progredire istPointer,
-- altrimenti gli assegna il valore in cima alla stack
execClose :: Program -> Program
execClose prog = prog {
        istPointer = newPtr,
        stack = tail st } where
            newPtr = if close then istPtr + 1 else head st
            close = (mem !! memPtr) == 0
            istPtr = istPointer prog
            st = stack prog
            mem = memory prog
            memPtr = memPointer prog

execIn :: Program -> Program
execIn prog = prog{
    istPointer = succ $ istPointer prog,
    state = WaitIn}

execOut :: Program -> Program
execOut prog = prog{
    istPointer = succ $ istPointer prog,
    state = WaitOut}

execDebug :: Program -> Program
execDebug prog = prog {
    istPointer = succ $ istPointer prog,
    state = Debug }

getOutput :: Program -> Char
getOutput prog = chr . fromIntegral $ (memory prog) !! (memPointer prog)

giveInput :: Program -> Char -> Program
giveInput prog val = prog{
    memory = insertInMemory (memory prog) (memPointer prog) (fromIntegral . ord $ val)
}

execComment :: Program -> Program
execComment prog = prog { istPointer = succ $ istPointer prog }

resume :: Program -> Program
resume prog = prog { state = Run }

checkEnd :: Program -> Program
checkEnd prog = prog { state = if checkState then End else state prog } where
    checkState = (istPointer prog) >= (length $ code prog)

getCommand :: Program -> Char
getCommand prog = (code prog) !! (istPointer prog)

printDebug :: Program -> String
printDebug prog = 
    "codice: "++(show $ istPointer prog)++"->"++[(getCommand prog)]++"\n"++
    "memoria: "++(show $ memPointer prog)++"="++(show $ (memory prog)!!(memPointer prog))++"\n"++
    "pila: "++(show $ stack prog)++"\n\n"

getFunction :: Char -> (Program -> Program)
getFunction ist = case ist of
    '+' -> execAdd
    '-' -> execSub
    '>' -> execRight
    '<' -> execLeft
    '[' -> execOpen
    ']' -> execClose
    '.' -> execOut
    ',' -> execIn
    '!' -> execDebug
    _ -> execComment

execInstruction :: Program -> Program
execInstruction prog = (getFunction $ getCommand prog) prog

execProgram :: Program -> IO ()
execProgram prog = case state prog of
    Run -> do
        let prog' = checkEnd . execInstruction $ prog
        execProgram prog'
        return ()
    WaitIn -> do
        inVal <- getChar
        let prog' = giveInput prog inVal
        execProgram . resume $ prog'
        return ()
    WaitOut -> do
        let outVal = getOutput prog
        putChar outVal
        execProgram . resume $ prog
        return ()
    Debug -> do
            putStr $ printDebug prog
            execProgram . resume $ prog
            return ()
    End -> do return ()

main = do
    args <- getArgs
    if length args /= 2 then
        putStr "bfi.exe\n  -c {code}\n  -f {file}"
        else do
            let mode = args !! 0
            case mode of
                "-c" -> do
                    let program = newProgram $ args !! 1
                    execProgram program
                "-f" -> do
                    code <- readFile $ args!! 1
                    let program = newProgram code
                    execProgram program
                _ -> do
                    putStr "bfi.exe\n  -c {code}\n  -f {file}"

