{-=BasicVariantFilter (BVF): A Haskell-based solution to Lymphoma=-}
{-=project variants_merged.tsv files basic filtering=-}
{-=pipeline.=-}
{-=Author: Matthew Mosior=-}
{-=Version: 3.0=-}
{-=Synopsis:  This Haskell Script will take in=-} 
{-=a merged_variants.tsv and output a .tsv file=-} 
{-=to either stdout, or to a user-specified output file=-}
{-=with all of the variants that pass basic filtering.=-} 

{-Module-}

module Bvf where

{--------}

{-Lanuguage Extension.-}

{-# LANGUAGE MultiWayIf #-}

{----------------------}

{-Imports-}

import Control.Arrow as CA
import Control.Monad as CM
import Data.Char as DC
import Data.List as DL
import Data.List.Split as DLS
import Data.Tuple as DT
import System.Console.GetOpt as SCG
import System.Environment as SE
import System.Exit as SX
import System.IO as SIO
import System.IO.Temp as SIOT
import System.Process as SP
import Text.PrettyPrint.Boxes as TPB
import Text.Regex.TDFA as TRP

{---------} 


{-Custom CML Option Datatype.-}

data SH =
    SHES
  | SHSH
  | SHST
  deriving (Eq,Show)

{-----------------------------}

{-General Utility Functions.-}

--isSubsetOf -> This function will
--be used in the stripHeader function.
xs `isSubsetOf` ys = any (`elem` ys) xs

--lineFeed -> This function will
--read the file in and split on
--whitespace, returning a list
--of lists.
lineFeed :: String -> [[String]]
lineFeed [] = []
lineFeed xs = DL.map DL.words (DL.lines xs)

--isAlphaList -> This function will
--test a string for only alphabetic
--characters.
isAlphaList :: String -> Bool
isAlphaList xs = all DC.isAlpha xs

--isNotAlphaList -> This function will
--test a String for non-alphabetic
--characters.
isNotAlphaList :: String -> Bool
isNotAlphaList xs = not (all DC.isAlpha xs) 

--mapNotLast -> This function will
--work like the traditional map 
--function in Data.List, but not
--map to the last element of a list.
mapNotLast :: (a -> a) -> [a] -> [a]
mapNotLast fn []     = []
mapNotLast fn [x]    = [x]
mapNotLast fn (x:xs) = fn x : mapNotLast fn xs

--mapTuple -> This function will
--map a function across all elements
--of a two-tuple.
mapTuple = CM.join (***)

--matchedReplication -> This function will
--take in two lists, and replicate items in 
--one list as long as the other list.
matchedReplication :: [[String]] -> [Int] -> [[Int]]
matchedReplication [] []         = []
matchedReplication _  []         = []
matchedReplication [] _          = []
matchedReplication (x:xs) (y:ys) = [DL.replicate (DL.length x) y] ++ (matchedReplication xs ys)

--nestedCycle -> This function will
--repeat a list of numbers the length
--of another list.
nestedCycle :: [[String]] -> [Int] -> [[Int]]
nestedCycle [] []     = []
nestedCycle _ []      = []
nestedCycle [] _      = []
nestedCycle (x:xs) ys = [DL.take (DL.length x) ys] ++ (nestedCycle xs ys)

--orderList -> This function will
--order a nested list.
orderList :: [[String]] -> [[Int]] -> [[Int]] -> [[(String,Int,Int)]]
orderList [] [] []             = []
orderList [] [] _              = []
orderList [] _  []             = []
orderList _  [] []             = []
orderList _  _  []             = []
orderList [] _  _              = [] 
orderList _  [] _              = []
orderList (x:xs) (y:ys) (z:zs) = [DL.zip3 x y z] ++ (orderList xs ys zs)

--tripletFst -> This function will
--act as a fst but for a triplet.
tripletFst :: (String,Int,Int) -> String
tripletFst (x,y,z) = x

--tuplifyTwo -> This function will
--turn a list of two elements into
--a two-tuple.
tuplifyTwo :: [a] -> (a,a)
tuplifyTwo [x,y]     = (x,y)

--customListFilter -> This function will
--perform a custom, regex-based filtration
--using the nested predicate functions.
customListFilter :: [String] -> [[(String,Int,Int)]] -> [(String,Int,Int)]
customListFilter [] _      = []
customListFilter _  []      = []
customListFilter xs (y:ys) = if smallCustomPredicate (DL.head xs) y
                                 then y
                                 else customListFilter xs ys
    where
        --Nested function definitions.--
        --smallCustomPredicate
        smallCustomPredicate :: String -> [(String,Int,Int)] -> Bool
        smallCustomPredicate []     _       = False
        smallCustomPredicate _      []      = False
        smallCustomPredicate x ((y,_,_):ys) = if (y =~ x :: Bool)
                                                  then True
                                                  else smallCustomPredicate x ys
        --------------------------------

--customOnlyDataFilter -> This function will
--perform a custom, regex-based partition
--using a custom predicate.
customOnlyDataFilter :: [String] -> [(String,Int,Int)] -> [(String,Int,Int)]
customOnlyDataFilter [] [] = []
customOnlyDataFilter [] _  = []
customOnlyDataFilter _  [] = []
customOnlyDataFilter xs ys = smallCustomFilter (DL.head xs) ys
     where 
         --Nested function definitions.--
         --smallCustomFilter
         smallCustomFilter :: String -> [(String,Int,Int)] -> [(String,Int,Int)]
         smallCustomFilter [] []     = []
         smallCustomFilter [] _      = []
         smallCustomFilter _  []     = []
         smallCustomFilter x  (y:ys) = if smallCustomPredicate x y
                                           then [y] ++ (smallCustomFilter x ys)
                                           else smallCustomFilter x ys
         --smallCustomPredicate
         smallCustomPredicate :: String -> (String,Int,Int) -> Bool
         smallCustomPredicate [] ([],_,_) = False
         smallCustomPredicate _  ([],_,_) = False
         smallCustomPredicate [] _        = False
         smallCustomPredicate x  (y,_,_)  = if (y =~ x :: Bool)
                                                then False 
                                                else True 
         --------------------------------

--customNotDataFilter -> This function will
--perform a custom, regex-based partition
--using a custom predicate.
customNotDataFilter :: [String] -> [(String,Int,Int)] -> [(String,Int,Int)]
customNotDataFilter [] [] = []
customNotDataFilter [] _  = []
customNotDataFilter _  [] = []
customNotDataFilter xs ys = smallCustomFilter (DL.head xs) ys
    where
        --Nested function definitions.--
        --smallCustomFilter
        smallCustomFilter :: String -> [(String,Int,Int)] -> [(String,Int,Int)]
        smallCustomFilter [] []     = []
        smallCustomFilter [] _      = []
        smallCustomFilter _  []     = []
        smallCustomFilter x  (y:ys) = if smallCustomPredicate x y
                                         then [y] ++ (smallCustomFilter x ys)
                                         else smallCustomFilter x ys
        --smallCustomPredicate
        smallCustomPredicate :: String -> (String,Int,Int) -> Bool
        smallCustomPredicate [] ([],_,_) = False
        smallCustomPredicate _  ([],_,_) = False
        smallCustomPredicate [] _        = False
        smallCustomPredicate x  (y,_,_)  = if (y =~ x :: Bool)
                                               then True
                                               else False
        --------------------------------

--customSplit -> This function will
--split a input by the basis of each
--characters character class.
customSplit :: String -> [(String,String)]
customSplit [] = []
customSplit (x:xs) = if (DC.isDigit x) || (DC.isLower x) || (DC.isUpper x) || (x == '.') || (x == '-') || (x == ',') 
                         then [("",[x])] ++ customSplit xs
                         else [([x],"")] ++ customSplit xs

--customAggregate -> This function will
--aggregate the result of customSplit.
customAggregate :: [(String,String)] -> (String,String)
customAggregate [] = ([],[])
customAggregate xs = (DL.concat (DL.map (fst) xs),DL.concat (DL.map (snd) xs))

--singleunnest -> This function will
--unnest a list.
singleunnest :: [a] -> a
singleunnest [a] = a

--smallSort -> This function will
--perform sorting on lists of triplets.
smallSort :: [[(String,Int,Int)]] -> [[(String,Int,Int)]]
smallSort [] = []
smallSort (x:xs) = [DL.sortBy (\(_,_,a) (_,_,b) -> compare a b) x] ++ (smallSort xs)

--strongEq -> This function will
--serve as a stronger version of ==.
strongEq :: (Eq a) => [a] -> [a] -> Bool
strongEq x y = DL.null (x \\ y) && DL.null (y \\ x)

--equalityListCheck -> This function will
--serve to grab all elements for == filter.
equalityListCheck :: [String] -> [(String,Int,Int)] -> [[(String,Int,Int)]]
equalityListCheck _ []      = []
equalityListCheck [] _      = []
equalityListCheck (x:xs) ys = [smallEqualityListCheck x ys] ++ (equalityListCheck xs ys)
    where
        --Nested function definitions.--
        --smallEqualityListCheck
        smallEqualityListCheck :: String -> [(String,Int,Int)] -> [(String,Int,Int)]
        smallEqualityListCheck [] _      = []
        smallEqualityListCheck _ []      = []
        smallEqualityListCheck x  (y:ys) = if smallPredicate x (tripletFst y)
                                               then [y] ++ (smallEqualityListCheck x ys)
                                               else smallEqualityListCheck x ys
        --smallPredicate
        smallPredicate :: String -> String -> Bool
        smallPredicate [] _  = False
        smallPredicate _  [] = False
        smallPredicate x  y  = if x == y
                                   then True
                                   else False
        --------------------------------

{----------------------------}


{-FilterFields Functions.-}

--ffgenerator -> This function will
--generate the list of acceptable
--numbers of filterfield delimiters.
ffgenerator :: [Int] -> [Int]
ffgenerator xs = xs ++ ffgenerator (DL.map (\n -> n + 4) xs)

--filterFieldsCheck -> This function will
--check for the appropriate number of 
--delimiters present in FilterFields.
filterFieldsCheck :: String -> Bool
filterFieldsCheck xs = if (DL.elem flaglength ffdelimiterlengths) && ((DL.filter (flip DL.elem ";:~") xs) == ffdelimitercycler)
                           then True
                           else False
    where
        flaglength         = DL.length (DL.filter (flip DL.elem ";:~") xs)
        ffdelimiterlengths = DL.take (DL.length (DL.filter (flip DL.elem ";:~") xs))
                           $ ffgenerator [5]
        ffdelimitercycler = DL.concat (DL.take (DL.length (DL.filter (flip DL.elem ";:~") xs)) 
                            (cycle [";",":","~","~"]))

--indexAdder -> This function will 
--add indexes to the input list.
indexAdder :: [[String]] -> [[(String,Int,Int)]]
indexAdder [] = []
indexAdder xs = orderList xs (matchedReplication xs [0..(DL.length xs - 1)]) (nestedCycle xs [0..])

--specificFilters -> This function will
--applied the prepared specific filtration
--elucidated by filterFields.
specificFilters :: [[String]] -> [[(String,Int,Int)]] -> [[(String,Int,Int)]]
specificFilters [] [] = []
specificFilters [] _  = []
specificFilters _  [] = []
specificFilters (x:xs) ys = do
    --Grab the sublist of ys that matches x (on head).
    let matchedys = customListFilter x ys
    --Grab the entire portion of matchedys that isn't the column header.
    let onlydata = customOnlyDataFilter x matchedys 
    --Grab the entire portion of matchedys that is the column header.
    let notdata = customNotDataFilter x matchedys
    --Grab !! 1 of x.
    let onex = x DL.!! 1
    --Grab !! 2 of x.
    let twox = x DL.!! 2 
    --Grab !! 3 of x.
    let threex = x DL.!! 3
    --Grab the comparision tuple from threex.
    let threexcomparison = customAggregate (customSplit threex)   
    --Walk through all possibilities of entire field of delimiters.
    if | (isNotAlphaList onex) && 
         (DL.elem '+' twox) && 
         (DL.isInfixOf ">=" threex) && 
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "x") ->
         [((notdata) ++ 
          (DL.filter (\(y,_,_) -> (DT.uncurry (+) (mapTuple (read :: String -> Int) (tuplifyTwo (DLS.splitOneOf ";:," y)))) >= 
          read (snd threexcomparison)) onlydata))] ++ (specificFilters xs ys)
       | (isNotAlphaList onex) && 
         (DL.elem '/' twox) && 
         (DL.isInfixOf ">=" threex) && 
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "x") ->
         [((notdata) ++ 
          (DL.filter (\(y,_,_) -> (DT.uncurry (/) (mapTuple (read :: String -> Double) (tuplifyTwo (DLS.splitOneOf ";:," y)))) >= 
          read (snd threexcomparison)) onlydata))] ++ (specificFilters xs ys)
       | (isNotAlphaList onex) && 
         (DL.elem '+' twox) && 
         (DL.isInfixOf ">=" threex) && 
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "y") ->
         [((notdata) ++ 
          (DL.filter (\(y,_,_) -> (DT.uncurry (+) (DT.swap (mapTuple (read :: String -> Int) (tuplifyTwo (DLS.splitOneOf ";:," y))))) >= 
          read (snd threexcomparison)) onlydata))] ++ (specificFilters xs ys)
       | (isNotAlphaList onex) && 
         (DL.elem '/' twox) && 
         (DL.isInfixOf ">=" threex) && 
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "y") ->
         [((notdata) ++ 
          (DL.filter (\(y,_,_) -> (DT.uncurry (/) (DT.swap (mapTuple (read :: String -> Double) (tuplifyTwo (DLS.splitOneOf ";:," y))))) >= 
          read (snd threexcomparison)) onlydata))] ++ (specificFilters xs ys)
       | (isNotAlphaList onex) && 
         (DL.elem '+' twox) && 
         (DL.isInfixOf "<=" threex) && 
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "x") ->
         [((notdata) ++ 
          (DL.filter (\(y,_,_) -> (DT.uncurry (+) (mapTuple (read :: String -> Int) (tuplifyTwo (DLS.splitOneOf ";:," y)))) <= 
          read (snd threexcomparison)) onlydata))] ++ (specificFilters xs ys)
       | (isNotAlphaList onex) && 
         (DL.elem '/' twox) && 
         (DL.isInfixOf "<=" threex) && 
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "x") ->
         [((notdata) ++ 
          (DL.filter (\(y,_,_) -> (DT.uncurry (/) (mapTuple (read :: String -> Double) (tuplifyTwo (DLS.splitOneOf ";:," y)))) <= 
          read (snd threexcomparison)) onlydata))] ++ (specificFilters xs ys)
       | (isNotAlphaList onex) && 
         (DL.elem '+' twox) && 
         (DL.isInfixOf "<=" threex) && 
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "y") -> 
         [((notdata) ++ 
          (DL.filter (\(y,_,_) -> (DT.uncurry (+) (DT.swap (mapTuple (read :: String -> Int) (tuplifyTwo (DLS.splitOneOf ";:," y))))) <= 
          read (snd threexcomparison)) onlydata))] ++ (specificFilters xs ys)
       | (isNotAlphaList onex) && 
         (DL.elem '/' twox) && 
         (DL.isInfixOf "<=" threex) && 
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "y") ->
         [((notdata) ++ 
          (DL.filter (\(y,_,_) -> (DT.uncurry (/) (DT.swap (mapTuple (read :: String -> Double) (tuplifyTwo (DLS.splitOneOf ";:," y))))) <= 
          read (snd threexcomparison)) onlydata))] ++ (specificFilters xs ys)
       | (isNotAlphaList onex) && 
         (DL.elem '-' twox) && 
         (DL.isInfixOf ">=" threex) && 
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "x") -> 
         [((notdata) ++ 
          (DL.filter (\(y,_,_) -> (DT.uncurry (-) (mapTuple (read :: String -> Int) (tuplifyTwo (DLS.splitOneOf ";:," y)))) >= 
          read (snd threexcomparison)) onlydata))] ++ (specificFilters xs ys)
       | (isNotAlphaList onex) && 
         (DL.elem '-' twox) && 
         (DL.isInfixOf ">=" threex) && 
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "y") ->
         [((notdata) ++ 
          (DL.filter (\(y,_,_) -> (DT.uncurry (-) (DT.swap (mapTuple (read :: String -> Int) (tuplifyTwo (DLS.splitOneOf ";:," y))))) >= 
          read (snd threexcomparison)) onlydata))] ++ (specificFilters xs ys)
       | (isNotAlphaList onex) && 
         (DL.elem '-' twox) && 
         (DL.isInfixOf "<=" threex) && 
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "x") ->
         [((notdata) ++ 
          (DL.filter (\(y,_,_) -> (DT.uncurry (-) (mapTuple (read :: String -> Int) (tuplifyTwo (DLS.splitOneOf ";:," y)))) <= 
         read (snd threexcomparison)) onlydata))] ++ (specificFilters xs ys)
       | (isNotAlphaList onex) && 
         (DL.elem '-' twox) && 
         (DL.isInfixOf "<=" threex) && 
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "y") ->
         [((notdata) ++ 
          (DL.filter (\(y,_,_) -> (DT.uncurry (-) (DT.swap (mapTuple (read :: String -> Int) (tuplifyTwo (DLS.splitOneOf ";:," y))))) >= 
          read (snd threexcomparison)) onlydata))] ++ (specificFilters xs ys)
       | (isNotAlphaList onex) && 
         (DL.elem '|' twox) && 
         (DL.isInfixOf ">=" threex) && 
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "x") && 
         (snd (tuplifyTwo (DLS.splitOn "," onex)) == "_") ->
         [((notdata) ++ 
          (DL.filter (\(y,_,_) -> (read :: String -> Int) (fst (tuplifyTwo (DLS.splitOneOf ";:," y))) >= 
          read (snd threexcomparison)) onlydata))] ++ (specificFilters xs ys)
       | (isNotAlphaList onex) && 
         (DL.elem '|' twox) && 
         (DL.isInfixOf ">=" threex) && 
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "_") && 
         (snd (tuplifyTwo (DLS.splitOn "," onex)) == "y") ->
         [((notdata) ++ 
          (DL.filter (\(y,_,_) -> (read :: String -> Int) (snd (tuplifyTwo (DLS.splitOneOf ";:," y))) >= 
          read (snd threexcomparison)) onlydata))] ++ (specificFilters xs ys)
       | (isNotAlphaList onex) && 
         (DL.elem '|' twox) && 
         (DL.isInfixOf "<=" threex) && 
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "x") && 
         (snd (tuplifyTwo (DLS.splitOn "," onex)) == "_") ->
         [((notdata) ++ 
          (DL.filter (\(y,_,_) -> (read :: String -> Int) (fst (tuplifyTwo (DLS.splitOneOf ";:," y))) <= 
          read (snd threexcomparison)) onlydata))] ++ (specificFilters xs ys)
       | (isNotAlphaList onex) && 
         (DL.elem '|' twox) && 
         (DL.isInfixOf "<=" threex) && 
         ((fst (tuplifyTwo (DLS.splitOn "," onex))) == "_") && 
         (snd (tuplifyTwo (DLS.splitOn "," onex)) == "y") ->
         [((notdata) ++ 
          (DL.filter (\(y,_,_) -> (read :: String -> Int) (snd (tuplifyTwo (DLS.splitOneOf ";:," y))) <= 
          read (snd threexcomparison)) onlydata))] ++ (specificFilters xs ys)
       | (isAlphaList onex) && 
         (DL.elem '|' twox) && 
         (DL.isInfixOf ">=" threex) ->
         [((notdata) ++ 
          ((DL.filter (\(y,_,_) -> ((read :: String -> Double) y) >= 
          (read :: String -> Double) (snd threexcomparison)) onlydata)))] ++ (specificFilters xs ys)
       | (isAlphaList onex) && 
         (DL.elem '|' twox) && 
         (DL.isInfixOf "<=" threex) ->
         [((notdata) ++ ((DL.filter (\(y,_,_) -> ((read :: String -> Double) y) <= 
          (read :: String -> Double) (snd threexcomparison)) onlydata)))] ++ (specificFilters xs ys)
       | (isAlphaList onex) && 
         (DL.elem '|' twox) && 
         (DL.isInfixOf "==" threex) ->
         [((notdata) 
          ++ (DL.concat (equalityListCheck (DLS.splitOneOf "," (snd threexcomparison)) onlydata)))]
          ++ (specificFilters xs ys)
       | otherwise -> specificFilters xs ys
 
--addNonFilters -> This function will
--add back the non-filtered fields.
addNonFilters :: [[String]] -> [[(String,Int,Int)]] -> [[(String,Int,Int)]] -> [[(String,Int,Int)]]
addNonFilters []     []    (_:_) = []
addNonFilters []     (_:_) _     = []
addNonFilters []     []    []    = []
addNonFilters (x:xs) ys    zs    = ((regexFilter x ys) ++ addNonFilters xs ys zs) ++ zs
    where
        --Nested Function Definitions.--
        --regexFilter
        regexFilter :: [String] -> [[(String,Int,Int)]] -> [[(String,Int,Int)]]
        regexFilter []     _  = []
        regexFilter _      [] = []
        regexFilter x (y:ys)  = if regexPredicate (DL.head x) y
                                    then [y] ++ regexFilter x ys
                                    else regexFilter x ys
        --regexPredicate
        regexPredicate :: String -> [(String,Int,Int)] -> Bool
        regexPredicate [] _  = False
        regexPredicate _ []  = False
        regexPredicate x  ys = if DL.any (\(a,_,_) -> a =~ x :: Bool) ys
                                        then False
                                        else True 
        --------------------------------

--reorderList -> This function will
--reorder a list based on another list.
reorderList :: [[(String,Int,Int)]] -> [[(String,Int,Int)]] -> [[(String,Int,Int)]]
reorderList [] [] = []
reorderList [] _  = []
reorderList _  [] = []
reorderList xs ys = DL.concatMap (\a -> DL.filter (\(b:bs) -> (tripletFst b) == (tripletFst a))  ys) (DL.map (DL.head) xs)

--processFilteredList -> This function will
--walk through a filtered and unfiltered list
--and determine necessary processing.
processFilteredList :: [[(String,Int,Int)]] -> [[(String,Int,Int)]] -> [[String]]
processFilteredList [] [] = []
processFilteredList xs ys = DL.map (DL.map (tripletFst)) (DL.map (fst) (DL.filter (\(a,b) -> strongEq a b) (DL.zip (xs) (smallSort (DL.groupBy (\(_,a,_) (_,b,_) -> a == b) (DL.sortBy (\(_,a,_) (_,b,_) -> compare a b) (DL.concat ys)))))))

--filterFields -> This function will
--filter a field by the corresponding
--field.
filterFields :: String -> [[String]] -> [[String]]
filterFields []    []    = []
filterFields ff    xs    = --Check to see if ff /= "NONE" (not default value).
                           if ff /= "NONE"
                               then do
                                   --Remove beginning and ending delimiters.
                                   let begendremoved = DL.init (DL.tail ff)
                                   --Push the separate filtrations into a list.
                                   let filteringlist = DLS.splitOn ";" begendremoved
                                   --Get the field separated from the filtration condition.
                                   let fieldandcondition = DL.map (DLS.splitOneOf ":~") filteringlist 
                                   --Add indexes to xs.
                                   let indexedxs = indexAdder xs
                                   --Call specificFilters on fieldandcondition. 
                                   let specificfiltered = specificFilters fieldandcondition (DL.transpose indexedxs)
                                   --Add back the nonfilteredlists.
                                   let nonfiltersadded = addNonFilters fieldandcondition (DL.transpose indexedxs) specificfiltered
                                   --Reorder nonfiltersadded.
                                   let reorderedlist = reorderList (DL.transpose indexedxs) nonfiltersadded
                                   --Process the transposed reorderedlist.
                                   processFilteredList indexedxs (DL.transpose reorderedlist)
                           --The default value, "NONE", was supplied.
                           else xs

{-------------------------}


{-stripHeader function.-}

--stripHeader -> This function will
--strip the headers from the input file.
stripHeader :: SH -> [[String]] -> [[String]]
stripHeader _ [] = []
stripHeader sh xs = case sh of
                        SHES -> DL.filter (not . ((DL.head xs) `isSubsetOf`)) xs
                        SHSH -> DL.filter (not . ((DL.tail (DL.head xs)) `isSubsetOf`)) xs
                        SHST -> DL.filter (not . ((DL.init (DL.head xs)) `isSubsetOf`)) xs

{-----------------------}


{-Printing functions.-}

--tempFileCreation -> This function will
--print the file to stdout using
--readProcess of the unix tool cat.
catFile :: [[String]] -> IO ()
catFile [] = return ()
catFile xs = do
    --Open a temporary file.
    (tempfile,temph) <- SIOT.openTempFile "." "temp.txt"
    --Intercalate a tab, and then a newline into xs.
    let intercalatedxs = DL.intercalate "\n" (DL.map (DL.intercalate "\t") xs)
    --Add intercalatedxs to temp.txt.
    hPutStrLn temph intercalatedxs
    --Close the temporary file's handle.
    hClose temph
    --Print out the contents of tempfile to the screen using cat unix tool.
    (_,_,_,ph) <- SP.createProcess (SP.proc "cat" [tempfile])
    ec <- SP.waitForProcess ph
    case ec of
        SX.ExitSuccess   -> do _ <- SP.readProcess "rm" [tempfile] []
                               return ()
        SX.ExitFailure _ -> do _ <- error "Could not cat file."
                               _ <- SP.readProcess "rm" [tempfile] []
                               return ()

--printFile -> This function will
--print the file to either stdout
--or to a output file based on
--command-lines options provided.
printFile :: String -> [[String]] -> IO ()
printFile [] [] = return ()
printFile [] _  = return ()
printFile _  [] = return ()
printFile outf xs = do
    --mapNotLast tabs and newlines in xs.
    let tabsandnewlinesadded = DL.map (mapNotLast (++ "\t")) xs
    --Write the output to the user-specified filename.
    SIO.writeFile (outf) $
                  (TPB.render $
                  (TPB.hsep 0 TPB.left . DL.map (TPB.vcat TPB.left) . DL.map (DL.map (TPB.text)))
                  (DL.transpose tabsandnewlinesadded)) 

{---------------------}


{-BVF Specific Function.-}

--processArgsAndFiles -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFiles :: (String,String,SH,FilePath) -> IO ()
processArgsAndFiles ([],[],_,[]) = return () 
processArgsAndFiles (ff,outputf,sh,inputfile) = do
    --Read in the file.
    readinputfile <- SIO.readFile inputfile
    --Apply lineFeed function to inputfile.
    let processedfile = lineFeed readinputfile 
    --Transpose the lines to group them correctly.
    let transposedfile = DL.transpose processedfile 
    --Filter the file based on the filter fields header. 
    let filteredfile = filterFields ff processedfile 
    --Strip the header out if it is supplied in the options.
    let strippedfile = stripHeader sh filteredfile  
    --Print the file to stdout (cat) or to a file.
    if outputf /= "NONE" 
        then printFile outputf strippedfile
        else catFile strippedfile

--processArgsAndContents -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContents :: (String,String,SH,FilePath) -> IO ()
processArgsAndContents ([],[],_,[]) = return ()
processArgsAndContents (ff,outputf,sh,content) = do
    --Apply lineFeed function to inputfile.
    let processedfile = lineFeed content
    --Transpose the lines to group them correctly.
    let transposedfile = DL.transpose processedfile
    --Filter the file based on the filter fields header.
    let filteredfile = filterFields ff processedfile
    --Strip the header out if it is supplied in the options.
    let strippedfile = stripHeader sh filteredfile 
    --Print the file to stdout (cat) or to a file.
    if outputf /= "NONE"
        then printFile outputf strippedfile
        else catFile strippedfile

{-------------------------}
