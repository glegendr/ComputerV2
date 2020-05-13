module Compute (isCompute, checkCompute, computeMe) where

import Data
import Data.List
import Token
import Polish
import Bracket
import Var
import Data.HashMap.Strict as Hm (HashMap, member, (!))
import Parsing
import CalcExpo

printEnd :: [Token] -> String
printEnd tkLst =
    let newLst = intersperse (Op Add) $ reverse $ sortOn (\(Numb _ e) -> e) $ filter isNumbNotNull tkLst
        retList = case newLst of
            [] -> [Numb 0 0]
            _ -> newLst
        expo = getExpo retList
        rf = "Reduced Form: " ++ showTkList0 (makeItRedable retList) ++ "\n"
    in
        if (expo < 0 || expo > 2)
        then rf ++ "  The polynomial degree is " ++ show expo ++ ". I can't solve"
        else rf ++ calcX retList

isCompute :: [Token] -> Bool
isCompute [] = False
isCompute lst =
    let aft = dropWhile (/= (Op Equal)) lst
    in case aft of
        [] -> False
        _ -> if (last aft == (Var "?"))
            then True
            else False


getCUnknownName :: [Compute] -> String
getCUnknownName [] = []
getCUnknownName ((CUnknown x):_) = x
getCUnknownName (x:xs) = getCUnknownName xs

tokenToCompute :: Token -> Compute
tokenToCompute a@(Numb _ _) = CTk a
tokenToCompute (Op a) = COp a
tokenToCompute _ = CUnknown ""

checkCompute :: [Token] -> HashMap String Var -> IO [Compute]
checkCompute [] _ = return []
checkCompute lst hm
    | (Var "i") `elem` lst || any (Var.isIma hm) lst || (Var "[") `elem` lst || any (Var.isMat hm) lst = do
        ret <- checkType ((Var "z"):(Op Equal):lst) hm
        case ret of
            Void -> return [CUnknown "Error: Computation failed"]
            _ -> return $ [CVar (toVar ret lst hm)]
    | otherwise = do
        ret <- checkType ((Var "z"):(Op OpenBracket):(Var "x"):(Op CloseBracket):(Op Equal):lst) hm
        case ret of
            Fct _ _ tk -> return $ case (toVar ret lst hm) of
                Fct _ _ tk -> map tokenToCompute tk
                _ -> [CUnknown "Error: Computation failed"]
            _ -> return [CUnknown "Error: Computation failed"]

computeMe :: [Compute] -> [Compute] -> String
computeMe (x:[]) [] = show x
computeMe (x:[]) (y:[])
    | isImaCompute x || isImaCompute y || isMatCompute x || isMatCompute y = show $ x == y
computeMe bef aft =
        let solved = solvePolish $ delBracket $ smallReduce $ allLeft (computeToToken bef ++ [Op Equal] ++ computeToToken aft) False
        in  if (any (== UnParsed) solved)
            then "Can't solve this problem"
            else printEnd $ intersperse (Op Add) $ filter isNumbNotNull $ addAll $ filter isNumbNotNull solved

computeToToken :: [Compute] -> [Token]
computeToToken = computeToToken'' . computeToToken'
    where
        computeToToken' :: [Compute] -> [Token]
        computeToToken' [] = []
        computeToToken' ((CVar (Fct _ _ tkLst)):xs) = (Op OpenBracket) : tkLst ++ [Op CloseBracket] ++ computeToToken' xs
        computeToToken' ((CTk x):xs) = x : computeToToken' xs
        computeToToken' ((COp x):xs) = Op x : computeToToken' xs
        computeToToken'' :: [Token] -> [Token]
        computeToToken'' [] = []
        computeToToken'' (a@(Op CloseBracket):b@(Op OpenBracket):xs) = a : Op Mult : b : computeToToken'' xs
        computeToToken'' (x:xs) = x : computeToToken'' xs

toMatNoEnd :: [Token] -> HashMap String Var -> Compute
toMatNoEnd lst hm =
    let cut = endOfMat lst 0
        mat = toMat (take cut lst) hm
    in CVar (Mat "tmpMat" mat)

isImaCompute :: Compute -> Bool
isImaCompute (CVar (Ima _ _)) = True
isImaCompute x = False

isMatCompute :: Compute -> Bool
isMatCompute (CVar (Mat _ _)) = True
isMatCompute x = False