import System.Environment
import Data.List
import Token
import CalcExpo
import Parsing
import System.Exit
import Data.Char
import System.IO
import Var
import Data
import Data.HashMap.Strict as Hm (HashMap, empty, insert, foldl')
import ListNM

printEnd :: [Token] -> IO ()
printEnd tkLst = do
    let newLst = intersperse (Op Add) $ reverse $ sortOn (\(Numb _ e) -> e) $ filter (\x -> case x of
            Numb 0 _ -> False
            Numb _ _ -> True
            _ -> False) tkLst
    let expo = getExpo newLst
    putStrLn $ "Reduced Form: " ++ showTkList0 newLst
    if (expo < 0 || expo > 2)
    then putStrLn $ "The polynomial degree is " ++ show expo ++ ". I can't solve"
    else calcX newLst

getActFromUser :: HashMap String Var -> IO ()
getActFromUser hm = do
    putStr "> "
    hFlush stdout
    l <- getLine
    matchMove l
    where
        matchMove :: String -> IO ()
        matchMove x
            | str == "quit" = exitWith ExitSuccess
            | str == "list" = do
                putStr $ showVarMap hm
                getActFromUser hm
            | str == "help" = do
                putStrLn "THIS IS HELP"
                getActFromUser hm
            | otherwise = do
                let tk = stringTotokenLst x
                ret <- checkType tk hm
                case ret of
                    Void -> getActFromUser hm
                    _ -> do
                        let tmp = toVar tk hm
                        putVarLn tmp
                        getActFromUser $ Hm.insert (getName tmp) tmp hm
            where str = map toLower x  

transformX :: [Token] -> [Token]
transformX [] = []
transformX ((Var "x"):xs) = (Numb 1 1) : transformX xs
transformX (x:xs) = x : transformX xs

delSameExpo :: Token -> [Token] -> [Token]
delSameExpo _ [] = []
delSameExpo x@(Numb _ a) (y@(Numb _ b):xs)
    | a == b = delSameExpo x xs
    | otherwise = y : delSameExpo x xs
delSameExpo _ _ = [UnParsed]

addAll :: [Token] -> [Token]
addAll [] = []
addAll (x:xs) = foldl addToken x xs : addAll (delSameExpo x xs)

isNotOp = not . isOp

makeItRedable :: [Token] -> [Token]
makeItRedable = changeOp . intersperse (Op Add) . reverse . sortOn (\(Numb _ x) -> x) . addAll . filter isNotOp . toPositiv
    where
        changeOp :: [Token] -> [Token]
        changeOp [] = []
        changeOp (a@(Op _):b@(Numb x _):xs)
            | x < 0 = Op Minus : appMinus b : changeOp xs
            | otherwise = a : b : changeOp xs
        changeOp (x:xs) = x : changeOp xs

main = do
    -- getActFromUser empty
    args <- getArgs
    putStrLn $ showTkListName "x" $ makeItRedable $ delBracket $ smallReduce $ transformX $ stringTotokenLst $ args !! 0
    -- case args of
    --     [] -> putStrLn "Please give me an input"
    --     (x:[]) ->  printEnd $ getAll x
    --     _ -> putStrLn "To mutch input given"