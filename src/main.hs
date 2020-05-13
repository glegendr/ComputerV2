import System.Environment
import Data.List
import Token
import Parsing
import System.IO
import Var
import Data
import Data.List.Split (splitOn)
import Data.HashMap.Strict as Hm (HashMap, empty, insert, foldl', member, (!), delete)
import Bracket
import Compute
import CalcExpo
import Text.Printf
import Commands
import Data.Char (toLower)

getActFromUser :: HashMap String Var -> [String] -> IO ()
getActFromUser hm history = do
    putStr "> "
    hFlush stdout
    l <- getLine
    matchMove l
    where
        matchMove :: String -> IO ()
        matchMove x
            | str == "" = getActFromUser hm history
            | head optionStr `elem` commandsList = do
                (newHm, newHist) <- matchCommand optionStr hm history newX
                getActFromUser newHm newHist 
            | length (filter (== '=') x) > 1 = do
                putStrLn $ "Error: Multiple equal"
                getActFromUser hm $ history ++ [newX]
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
                                getActFromUser hm $ history ++ [newX]
                            (_, [CUnknown err]) -> do
                                putStrLn err
                                getActFromUser hm $ history ++ [newX]
                            ([], _) -> getActFromUser hm $ history ++ [newX]
                            _ -> do 
                                let print = "  " ++ computeMe ret ret2
                                putStrLn print
                                getActFromUser hm $ history ++ [newX])
                else (
                    do
                        ret <- checkType tk hm
                        case ret of
                            Void -> getActFromUser hm $ history ++ [newX]
                            _ -> do
                                let tmp = toVar ret ((tail $ dropWhile (/= (Op Equal)) tk)) hm
                                checkError tmp)
            where
                newX = x
                str = map toLower x
                optionStr = splitOn ":" str
                checkError :: Var -> IO ()
                checkError Void = do
                    let print = "Error: Unknown input"
                    putStrLn print
                    getActFromUser hm $ history ++ [newX]
                checkError var = do
                    putVarLn var
                    getActFromUser (Hm.insert (getName var) var hm) $ history ++ [newX]

main = do
    args <- getArgs
    if (any (\x -> x == "--help" || x == "-h") args)
    then do
        putStrLn $ helper []
        getActFromUser (addConst empty) []
    else getActFromUser (addConst empty) []