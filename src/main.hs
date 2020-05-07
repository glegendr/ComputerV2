import System.Environment
import Data.List
import Token
import Parsing
import System.Exit
import Data.Char
import System.IO
import Var
import Data
import Data.List.Split (splitOn)
import Data.HashMap.Strict as Hm (HashMap, empty, insert, foldl', member, (!), delete)
import Bracket
import Compute
import CalcExpo
import Text.Printf

delVar :: [String] -> HashMap String Var -> IO (HashMap String Var)
delVar [] hm = return hm
delVar (x:xs) hm
    | member x hm = do
        putStrLn $ "  Var \""++ x ++ "\" has been deleted"
        delVar xs (Hm.delete x hm)
    | x == "all" = do
        putStrLn "  Everything has been deleted"
        return empty
    | otherwise = do
        putStrLn $ "  Var \"" ++ x ++ "\" not found"
        delVar xs hm

replaceName :: String -> Var -> Var
replaceName name (Rat _ v) = Rat name v
replaceName name (Ima _ v) = Ima name v
replaceName name (Mat _ v) = Mat name v
replaceName name (Fct _ v tk) = Fct name v tk
replaceName name Void = Void

replaceVar :: [String] -> HashMap String Var -> IO (HashMap String Var)
replaceVar [] hm = return hm
replaceVar (x:x1:xs) hm
    | member x hm && member x1 hm = do
        let a = hm ! x
        let rX = (replaceName x1 a)
        let rX1 = (replaceName x $ hm ! x1)
        putStrLn $ "  " ++ show rX ++ " <-> " ++ show rX1
        replaceVar xs $ Hm.insert x1 rX $ Hm.insert x rX1 hm
    | member x hm = do
        replaceVar xs $ Hm.delete x $ Hm.insert x1 (replaceName x1 $ hm ! x) hm
    | otherwise = do
        putStrLn $ "  Can't replace \"" ++ x ++ "\" by \"" ++ x1 ++ "\""
        replaceVar xs hm
replaceVar (x:[]) hm = do
    putStrLn $ "  No value to replace \"" ++ x ++ "\""
    return hm

printHistory :: [(String, Int)] -> IO ()
printHistory [] = return ()
printHistory ((x, n):xs) = do
    printf "%-4i %s\n" n x
    printHistory xs


toInt :: String -> Int
toInt [] = 9999
toInt s
    | any (\x -> not $ isNumber x) s = 9999
    | otherwise = read s

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
            | str == "quit" = exitWith ExitSuccess
            | head optionStr == "list" = do
                putStr $ showVarMap hm (tail optionStr)
                getActFromUser hm history
            | head optionStr == "help" = do
                putStrLn $ helper (tail optionStr)
                getActFromUser hm  history
            | head optionStr == "del" = do 
                newHm <- delVar (tail optionStr) hm
                getActFromUser newHm $ history ++ [newX]
            | head optionStr == "replace" = do 
                newHm <- replaceVar (tail optionStr) hm
                getActFromUser newHm $ history ++ [newX]
            | head optionStr == "history" = do
                printHistory $ zip (reverse (take (toInt $ last optionStr) (reverse history))) [1..]
                getActFromUser hm history
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
                -- checkError (Ima _ x)
                --     | getExpo x > 1 || getExpo x < 0 = do
                --         let print =  "Error: powered i is invalid"
                --         putStrLn print
                --         getActFromUser hm $ history ++ [newX]
                checkError Void = do
                    let print = "Error: Unknown input"
                    putStrLn print
                    getActFromUser hm $ history ++ [newX]
                checkError var = do
                    putVarLn var
                    getActFromUser (Hm.insert (getName var) var hm) $ history ++ [newX]

main = do
    getActFromUser empty []
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

helper :: [String] -> String
helper []  = "  Helper:" ++ helper' ["c","o","?"]
helper lst = "  Helper:" ++ helper' lst

helper' :: [String] -> String
helper' [] = []
helper' (x:xs)
    | x == "c" || x == "commands"                = commandsHelp ++ helper' xs
    | x == "o" || x == "ope" || x == "operators" = operatorsHelp ++ helper' xs 
    | x == "?" || x == "computation"             = computationHelp ++ helper' xs
    | otherwise                                  = helper' xs 

commandsHelp =
    let ini = "\n  + Commands:"
        fm1 = "\n  | + (commandName:commandArgs):"
        hp1 = "\n  | | + help: Print this message"
        hp2 = "\n  | | | <c>/<commands>: Print command help"
        hp7 = "\n  | | | <o>/<ope>/<operators>: Print operators help"
        hp8 = "\n  | | | <?>/<computation>: Print computation help"
        ls1 = "\n  | | + list: List all existing var"
        ls2 = "\n  | | | <r>/<rat>/<rationals>: List all exsting rationals"
        ls3 = "\n  | | | <i>/<ima>/<imaginary>: List all exsting imaginary"
        ls4 = "\n  | | | <m>/<mat>/<matrix>: List all exsting matrix"
        ls5 = "\n  | | | <f>/<fct>/<functions>: List all exsting functions"
        ls6 = "\n  | | | <a>/<all>: List all exsting var"
        hi1 = "\n  | | + history: List all given input except commands"
        hi2 = "\n  | | | <N>: List N last input"
        de1 = "\n  | | + del: Del var"
        de2 = "\n  | | | <X> del var with name X"
        re1 = "\n  | | + replace: Replace var name by an other"
        re2 = "\n  | | / <X:Y> Replace var name X by Y. If Y exist the 2 name are swapping"
        fm2 = "\n  | + (comandName):"
        qit = "\n  | | + quit: quit the program"
    in ini++fm1++hp1++hp2++hp7++hp8++ls1++ls2++ls3++ls4++ls5++ls6++hi1++hi2++de1++de2++re1++re2++fm2++qit

operatorsHelp =
    let ini = "\n  + Operators:"
        op1 = "\n  | (+): Add two Numbers with the same expodent"
        op2 = "\n  | (-): Subtract two Numbers with the same expodent"
        op3 = "\n  | (*): Multiply two Numbers"
        op4 = "\n  | (/): Divide two Numbers"
        op5 = "\n  | (^): Make Number1 power Number2"
        op6 = "\n  | (%): Modulo two Numbers"
        op7 = "\n  | (**): Apply matricial mult between two matrix"
    in ini++op1++op2++op3++op4++op5++op6++op7

computationHelp =
    let ini = "\n  + Computation:"
        fr1 = "\n  | (FirstPart = SecondPart ?): Try solving this equation"
        fr2 = "\n  | (FristPart = ?): Display reduce form of the FirstPart"
    in ini++fr1++fr2