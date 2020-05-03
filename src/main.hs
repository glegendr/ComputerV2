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
import Bracket

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
                        checkError tmp hm
            where str = map toLower x  

checkError (Ima _ x) hm
    | getExpo x > 1 || getExpo x < 0 = do
        putStrLn "Error: powered i is invalid"
        getActFromUser hm
checkError var hm = do
    putVarLn var
    getActFromUser $ Hm.insert (getName var) var hm

transformX :: [Token] -> [Token]
transformX [] = []
transformX ((Var "x"):xs) = (Numb 1 1) : transformX xs
transformX (x:xs) = x : transformX xs

main = do
    getActFromUser empty
    -- args <- getArgs
    -- let tkLst = smallReduce $ transformX $ stringTotokenLst $ args !! 0
    -- let pow = read $ args !! 1
    -- putStrLn $ showTkListName "x" tkLst
    --                  putStrLn $ showTkListName "x" $ makeItRedable $ multinomialResolution tkLst $ intToToken pow
    -- print $ makeItRedable $ delBracket tkLst
    -- case args of
    --     [] -> putStrLn "Please give me an input"
    --     (x:[]) ->  printEnd $ getAll x
    --     _ -> putStrLn "To mutch input given"