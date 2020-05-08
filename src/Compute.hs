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

checkCompute :: [Token] -> HashMap String Var -> IO [Compute]
checkCompute [] _ = return []
checkCompute lst hm
    | checkBracket lst 0 = return [CUnknown "Error: Missmatched bracket"]
    | checkMultNumber lst = return [CUnknown "Error: Missed operator"]
    | (length $ filter (== Op Equal) lst) > 0 = return [CUnknown "Error: Multiple Eq"]
    | checkFct hm lst /= "" = return [CUnknown ("Error: Unknown Function " ++ checkFct hm lst)]
    | any isCUnknown newLst = return [CUnknown ("Error: Unexpected Value \"" ++ getCUnknownName newLst ++ "\"")]
    | isIncompatible newLst = return [CUnknown "Error: I can't compute this"]
    | otherwise = return newLst
    where
        newLst = toComputeToken hm lst
        isIncompatible :: [Compute] -> Bool
        isIncompatible [] = False
        isIncompatible ((CTk _):(COp x):(CTk _):xs)
            | x == MatricialMult = True
            | otherwise = isIncompatible xs
        isIncompatible ((COp _):x@(COp OpenBracket):xs) = isIncompatible (x:xs)
        isIncompatible ((COp CloseBracket):x@(COp _):xs) = isIncompatible (x:xs)
        isIncompatible ((COp _):(COp _):xs) = True
        isIncompatible ((CVar (Mat _ _)):(COp x):(CVar (Mat _ _)):xs)
            | x == MatricialMult = isIncompatible xs
            | otherwise = True
        isIncompatible (x:xs) = isIncompatible xs
        checkFct :: HashMap String Var -> [Token] -> String
        checkFct hm [] = []
        checkFct hm ((Var name):(Op OpenBracket):(Numb _ _):(Op CloseBracket):xs) 
            | member name hm = case hm ! name of
                (Fct name _ _) -> checkFct hm xs
                _ -> name
            | otherwise = name
        checkFct hm ((Var name):(Op OpenBracket):(Op Minus):(Numb _ _):(Op CloseBracket):xs) 
            | member name hm = case hm ! name of
                (Fct name _ _) -> checkFct hm xs
                _ -> name
            | otherwise = name
        checkFct hm (x:xs) = checkFct hm xs

toComputeToken :: HashMap String Var -> [Token] -> [Compute]
toComputeToken hm lst = reduceCompute $ toComputeToken2 hm lst
    where
        reduceCompute :: [Compute] -> [Compute]
        reduceCompute [] = []
        reduceCompute (a@(COp OpenBracket):(COp Minus):(CTk b@(Numb x y)):xs) = a : CTk (appMinus b) : reduceCompute xs
        reduceCompute (a@(COp CloseBracket):b@(CTk _):xs) = a : COp Mult : b : reduceCompute xs
        reduceCompute (a@(COp CloseBracket):b@(CVar _):xs) = a : COp Mult : b : reduceCompute xs
        reduceCompute (a@(COp CloseBracket):b@(COp OpenBracket):xs) = a : COp Mult : reduceCompute (b:xs)
        reduceCompute (x:xs) = x : reduceCompute xs

        toComputeToken2 :: HashMap String Var -> [Token] -> [Compute]
        toComputeToken2 _ [] = []
        toComputeToken2 hm ((Var name):(Op OpenBracket):(Numb value _):(Op CloseBracket):xs) 
            | member name hm = case hm ! name of
                x@(Fct _ _ _) -> fctToCompute x value : toComputeToken2 hm xs
                _ -> error "This is not a function bro"
        toComputeToken2 hm ((Var name):(Op OpenBracket):(Op Minus):(Numb value _):(Op CloseBracket):xs) 
            | member name hm = case hm ! name of
                x@(Fct _ _ _) -> fctToCompute x (-value) : toComputeToken2 hm xs
                _ -> error "This is not a function bro"
        toComputeToken2 hm ((Var name):(Op OpenBracket):(Var name2):(Op CloseBracket):xs) 
            | member name hm && member name2 hm = case (hm ! name, hm ! name2) of
                (x@(Fct _ _ _), y@(Rat _ value)) -> fctToCompute x value : toComputeToken2 hm xs
                _ -> error "This is not a function bro"
            | member name hm && name2 == "x" = case hm ! name of
                x@(Fct _ _ _) -> CVar x : toComputeToken2 hm xs
                _ -> error "This is not a function bro"
        toComputeToken2 hm ((Var name):(Op OpenBracket):(Op Minus):(Var name2):(Op CloseBracket):xs) 
            | member name hm && member name2 hm = case (hm ! name, hm ! name2) of
                (x@(Fct _ _ _), y@(Rat _ value)) -> fctToCompute x (-value) : toComputeToken2 hm xs
                _ -> error "This is not a function bro"
        toComputeToken2 hm ((Var name):a@(Op OpenBracket):xs)
                | member name hm = case hm ! name of
                    (Rat _ x) -> CTk (Numb x 0) : COp Mult : toComputeToken2 hm (a:xs)
                    x         -> CVar x : toComputeToken2 hm xs
        toComputeToken2 hm ((Var name):xs)
                | member name hm = case hm ! name of
                    (Rat _ x) -> CTk (Numb x 0) : toComputeToken2 hm xs
                    x         -> CVar x : toComputeToken2 hm xs
        toComputeToken2 hm (x@(Var "["):xs) = toMatNoEnd (x:xs) hm : toComputeToken2 hm (drop (endOfMat (x:xs) 0 - 1) xs)
        toComputeToken2 hm ((Op x):xs) = COp x : toComputeToken2 hm xs
        toComputeToken2 hm (x@(Numb _ _):a@(Op OpenBracket):xs) = CTk x : COp Mult : toComputeToken2 hm (a:xs)
        toComputeToken2 hm (x@(Numb _ _):xs) = CTk x : toComputeToken2 hm xs
        toComputeToken2 hm (x:xs) = (CUnknown (show x)) : toComputeToken2 hm xs

fctToCompute :: Var -> Float -> Compute
fctToCompute (Fct _ _ tkLst) value = CTk (Numb (simpleReduce $ solvePolish $ delBracket $ smallReduce $ replaceFctVar value tkLst) 0)

computeMe :: [Compute] -> [Compute] -> String
computeMe (x:[]) [] = show x
computeMe lst []
    | any isMatCompute lst = "0"
    | any isImaCompute lst = "0"
    | otherwise            = showTkList $ makeItRedable $ solvePolish $ delBracket $ smallReduce $ computeToToken lst
computeMe bef aft
    | any isMatCompute bef || any isMatCompute aft || any isImaCompute bef || any isImaCompute aft = "Can't solve this problem"
    | otherwise = 
        let solved = solvePolish $ delBracket $ smallReduce $ allLeft (computeToToken bef ++ [Op Equal] ++ computeToToken aft) False
        in  if (any (== UnParsed) solved)
            then "Can't solve this problem"
            else printEnd $ intersperse (Op Add) $ filter isNumbNotNull $ addAll $ filter isNumbNotNull solved

isImaCompute :: Compute -> Bool
isImaCompute (CVar (Ima _ _)) = True
isImaCompute x = False

isMatCompute :: Compute -> Bool
isMatCompute (CVar (Mat _ _)) = True
isMatCompute x = False

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