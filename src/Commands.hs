module Commands (delVar, replaceVar, printHistory, helper, commandsList, matchCommand) where

import Data.List
import Data.HashMap.Strict as Hm (HashMap, empty, insert, foldl', member, (!), delete)
import Var
import Data
import Text.Printf
import System.Exit
import Data.Char (isNumber)

delVar :: [String] -> HashMap String Var -> IO (HashMap String Var)
delVar [] hm = return hm
delVar (x:xs) hm
    | x `elem` constName = do
        putStrLn $ "Error: Var \""++ x ++ "\" is a constant"
        delVar xs hm
    | member x hm = do
        putStrLn $ "  Var \""++ x ++ "\" has been deleted"
        delVar xs (Hm.delete x hm)
    | x == "a" || x == "all" = do
        putStrLn "  Everythings has been deleted"
        return (addConst empty)
    | x == "d" || x == "dup" || x == "duplicate" = do
        let newLst = delDup values
        putStrLn $ "  " ++ show ((length values) - (length newLst)) ++ " duplicate(s) have been deleted"
        delVar xs $ addConst $ foldl (\acc x -> Hm.insert (getName x) x acc) empty newLst
    | otherwise = do
        putStrLn $ "  Var \"" ++ x ++ "\" not found"
        delVar xs hm
    where
        names =  Hm.foldl' (\acc x -> (getName x) : acc) [] hm
        values =  filter (\x -> not $ x `elem` constList ) $ Hm.foldl' (\acc x -> x : acc) [] hm

delDup :: [Var] -> [Var]
delDup [] = []
delDup (x:xs) = x : delDup (delDup' x xs)

delDup' :: Var -> [Var] -> [Var]
delDup' _ [] = []
delDup' var (x:xs)
    | var == newX = delDup' var xs
    | otherwise = x : delDup' var xs
    where newX = replaceVarName x (getName var) (getVarName var)

replaceName :: String -> Var -> Var
replaceName name (Rat _ v) = Rat name v
replaceName name (Ima _ v) = Ima name v
replaceName name (Mat _ v) = Mat name v
replaceName name (Fct _ v tk) = Fct name v tk
replaceName name Void = Void

replaceVar :: [String] -> HashMap String Var -> IO (HashMap String Var)
replaceVar [] hm = return hm
replaceVar (x:x1:xs) hm
    | x `elem` constName =  do
        putStrLn $ "  Can't replace \"" ++ x ++ "\" by \"" ++ x1 ++ "\" because \"" ++ x ++ "\" is a constant"
        replaceVar xs hm
    | x1 == "i" || x1 `elem` constName = do
        putStrLn $ "  Can't replace \"" ++ x ++ "\" by \"" ++ x1 ++ "\" because \"" ++ x1 ++ "\" is a constant"
        replaceVar xs hm
    | member x hm && member x1 hm = do
        let a = hm ! x
        let rX = (replaceName x1 a)
        let rX1 = (replaceName x $ hm ! x1)
        putStrLn $ "  " ++ show rX ++ " <-> " ++ show rX1
        replaceVar xs $ Hm.insert x1 rX $ Hm.insert x rX1 hm
    | member x hm = do
        replaceVar xs $ Hm.delete x $ Hm.insert x1 (replaceName x1 $ hm ! x) hm
    | otherwise = do
        putStrLn $ "  \"" ++ x ++ "\" Not found"
        replaceVar xs hm
replaceVar (x:[]) hm = do
    putStrLn $ "  No value to replace \"" ++ x ++ "\""
    return hm

printHistory :: [(String, Int)] -> IO ()
printHistory [] = return ()
printHistory ((x, n):xs) = do
    printf "%-4i %s\n" n x
    printHistory xs


helper :: [String] -> String
helper []  = "  Helper:\n  + To use a command, use \"Name:Arg1:Arg2:...\"" ++ helper' ["h"]
helper lst = "  Helper:\n  + To use a command, use \"Name:Arg1:Arg2:...\"" ++ helper' (nub lst)

helper' :: [String] -> String
helper' [] = []
helper' (x:xs)
    | x == "h" || x == "help"                    = helpHelp ++ helper' xs
    | x == "c" || x == "commands"                = commandsHelp ++ helper' xs
    | x == "l" || x == "list"                    = listHelp ++ helper' xs
    | x == "i" || x == "history"                 = historyHelp ++ helper' xs
    | x == "d" || x == "del"                     = delHelp ++ helper' xs
    | x == "r" || x == "replace"                 = replaceHelp ++ helper' xs
    | x == "q" || x == "quit"                    = quitHelp ++ helper' xs
    | x == "o" || x == "ope" || x == "operators" = operatorsHelp ++ helper' xs 
    | x == "?" || x == "computation"             = computationHelp ++ helper' xs
    | otherwise                                  = helper' xs 

helpHelp =
    let ini = "\n  + help: Display help message"
        hp1 = "\n  | <h>/<help>: Display this message"
        hp2 = "\n  | <c>/<commands>: List all existing command"
        hp3 = "\n  | <l>/<list>: Display the command list"
        hp4 = "\n  | <i>/<history>: Display the command history"
        hp5 = "\n  | <d>/<del>: Display the command del"
        hp6 = "\n  | <r>/<replace>: Display the command replace"
        hp7 = "\n  | <q>/<quit>: Display the command quit"
        hp8 = "\n  | <o>/<ope>/<operators>: Display all operators"
        hp9 = "\n  | <?>/<computation>: Display how to compute"
    in ini++hp1++hp2++hp3++hp4++hp5++hp6++hp7++hp8++hp9

listHelp =
    let ini = "\n  + list: List all existing var"
        ls2 = "\n  | <r>/<rat>/<rationals>: List all exsting rationals"
        ls3 = "\n  | <i>/<ima>/<imaginary>: List all exsting imaginary"
        ls4 = "\n  | <m>/<mat>/<matrix>: List all exsting matrix"
        ls5 = "\n  | <f>/<fct>/<functions>: List all exsting functions"
        ls6 = "\n  | <c>/<cst>/<constants>: List all exsting constants"
        ls7 = "\n  | <a>/<all>: List all exsting var and constants"
    in ini++ls2++ls3++ls4++ls5++ls6++ls7

historyHelp =
    let ini = "\n  + history: List all given input except commands"
        hi1 = "\n  | <N>: List N last input"
    in ini++hi1

delHelp =
    let ini = "\n  + del: Del var"
        de2 = "\n  | <X> del var with name X"
        de3 = "\n  | <a>/<all> del all var"
        de4 = "\n  | <d>/<dup>/<duplicate> delete duplicates"
    in ini++de2++de3++de4

replaceHelp = 
    let ini = "\n  + replace: Replace var name by an other"
        re2 = "\n  | <X:Y> Replace var name X by Y. If Y exist the 2 name are swapping"
    in ini++re2

quitHelp = 
    "\n  + quit: quit the program"

commandsHelp =
    let ini = "\n  + Commands:"
        fm1 = "\n  | + (commandName:commandArgs):"
        hp0 = "\n  | | + help: Display help message"
        hp1 = "\n  | | | <h>/<help>: Display this message"
        hp2 = "\n  | | | <c>/<commands>: List all existing command"
        hp3 = "\n  | | | <l>/<list>: Display the command list"
        hp4 = "\n  | | | <i>/<history>: Display the command history"
        hp5 = "\n  | | | <d>/<del>: Display the command del"
        hp6 = "\n  | | | <r>/<replace>: Display the command replace"
        hp7 = "\n  | | | <q>/<quit>: Display the command quit"
        hp8 = "\n  | | | <o>/<ope>/<operators>: Display all operators"
        hp9 = "\n  | | | <?>/<computation>: Display how to compute"
        ls1 = "\n  | | + list: List all existing var"
        ls2 = "\n  | | | <r>/<rat>/<rationals>: List all exsting rationals"
        ls3 = "\n  | | | <i>/<ima>/<imaginary>: List all exsting imaginary"
        ls4 = "\n  | | | <m>/<mat>/<matrix>: List all exsting matrix"
        ls5 = "\n  | | | <f>/<fct>/<functions>: List all exsting functions"
        ls6 = "\n  | | | <c>/<cst>/<constants>: List all exsting constants"
        ls7 = "\n  | | | <a>/<all>: List all exsting var and constants"
        hi1 = "\n  | | + history: List all given input except commands"
        hi2 = "\n  | | | <N>: List N last input"
        de1 = "\n  | | + del: Del var"
        de2 = "\n  | | | <X> del var with name X"
        de3 = "\n  | | | <a>/<all> del every var"
        de4 = "\n  | | | <d>/<dup>/<duplicate> delete duplicates"
        re1 = "\n  | | + replace: Replace var name by an other"
        re2 = "\n  | | / <X:Y> Replace var name X by Y. If Y exist the 2 name are swapping"
        fm2 = "\n  | + (comandName):"
        qit = "\n  | | + quit: quit the program"
    in ini++fm1++hp0++hp1++hp2++hp3++hp4++hp5++hp6++hp7++hp8++hp9++ls1++ls2++ls3++ls4++ls5++ls6++ls7++hi1++hi2++de1++de2++de3++de4++re1++re2++fm2++qit

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


commandsList = ["quit", "list", "help", "del", "replace", "history"]

toInt :: String -> Int
toInt [] = 9999
toInt s
    | any (\x -> not $ isNumber x) s = 9999
    | otherwise = read s

matchCommand :: [String] -> HashMap String Var -> [String] -> String -> IO (HashMap String Var, [String])
matchCommand optionStr hm history newX
    | head optionStr == "quit" = exitWith ExitSuccess
    | head optionStr == "list" = do
        putStr $ showVarMap hm (tail optionStr)
        return (hm, history)
    | head optionStr == "help" = do
        putStrLn $ helper (tail optionStr)
        return (hm,  history)
    | head optionStr == "del" = do 
        newHm <- delVar (tail optionStr) hm
        return (newHm, history ++ [newX])
    | head optionStr == "replace" = do 
        newHm <- replaceVar (tail optionStr) hm
        return (newHm,  history ++ [newX])
    | head optionStr == "history" = do
        printHistory $ zip (reverse (take (toInt $ last optionStr) (reverse history))) [1..]
        return (hm, history)