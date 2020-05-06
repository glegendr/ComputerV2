import System.Environment
import Data.List
import Token
import Parsing
import System.Exit
import Data.Char
import System.IO
import Var
import Data
import Data.HashMap.Strict as Hm (HashMap, empty, insert, foldl')
import Bracket
import Compute
import CalcExpo

getActFromUser :: HashMap String Var -> IO ()
getActFromUser hm = do
    putStr "> "
    hFlush stdout
    l <- getLine
    matchMove l
    where
        matchMove :: String -> IO ()
        matchMove x
            | str == "" = getActFromUser hm
            | str == "quit" = exitWith ExitSuccess
            | str == "list" = do
                putStr $ showVarMap hm
                getActFromUser hm
            | str == "help" = do
                putStrLn "THIS IS HELP"
                getActFromUser hm
            | otherwise = do
                let tk = stringTotokenLst x
                if (isCompute tk)
                then (
                    do
                        ret <- checkCompute (takeWhile (/= (Op Equal)) tk) hm
                        ret2 <- checkCompute (init $ tail $ dropWhile (/= (Op Equal)) tk) hm
                        case (ret, ret2) of
                            ([CUnknown err], _) -> do
                                putStrLn err
                                getActFromUser hm
                            (_, [CUnknown err]) -> do
                                putStrLn err
                                getActFromUser hm
                            ([], _) -> getActFromUser hm
                            _ -> do 
                                putStrLn $ "  " ++ computeMe ret ret2
                                getActFromUser hm)
                else (
                    do
                        ret <- checkType tk hm
                        case ret of
                            Void -> getActFromUser hm
                            _ -> do
                                let tmp = toVar ret ((tail $ dropWhile (/= (Op Equal)) tk)) hm
                                checkError tmp hm)
            where str = map toLower x  

checkError (Ima _ x) hm
    | getExpo x > 1 || getExpo x < 0 = do
        putStrLn "Error: powered i is invalid"
        getActFromUser hm
checkError Void hm = do
        putStrLn "Error: Unknown input"
        getActFromUser hm
checkError var hm = do
    putVarLn var
    getActFromUser $ Hm.insert (getName var) var hm

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