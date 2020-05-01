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
                putStrLn $ Hm.foldl' (\acc x -> if acc == "" then acc ++ "  " ++ show x else acc ++ "\n  " ++ show x) "" hm
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

main = do
    getActFromUser empty
    -- args <- getArgs
    -- case args of
    --     [] -> putStrLn "Please give me an input"
    --     (x:[]) ->  printEnd $ getAll x
    --     _ -> putStrLn "To mutch input given"