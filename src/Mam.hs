{-=MoveAnnotateMerge: A Haskell-based solution to Lymphoma=-}
{-=project variants.annotated.tsv files moving and=-}
{-=annotation, and merging into a final file.=-}
{-=Author: Matthew Mosior=-}
{-=Version: Release=-}
{-=Synopsis:  This Haskell Script will take a file with=-}
{-=the names of directories and corresponding sample names=-}
{-=and a final directory and grab variants.annotated.tsv files=-}
{-=in each of the directories and will put then into the the=-}
{-=final directory and add the sample names to the files.=-}

{-Module-}

module Mam where

{--------}

{-Syntax Extensions.-}

{-# LANGUAGE OverloadedStrings #-}

{--------------------}


{-Imports-}

import Data.List as DL
import Data.List.Split as DLS
import System.Process as SP
import System.Environment as SE
import System.IO as SIO
import System.IO.Temp as SIOT
import Text.PrettyPrint.Boxes as TPB
import Text.Regex as TR

{---------} 


{-General Utility Functions.-}

--fileChunker -> This function will split the
--inputfile into discrete chunks.
fileChunker :: [String] -> [[String]]
fileChunker [] = []
fileChunker xs = DLS.chunksOf 3 xs

--forwardSlashDeletorNM -> This function will
--delete a forward slash at the end of a string.
forwardSlashDeletorNM :: String -> String
forwardSlashDeletorNM [] = []
forwardSlashDeletorNM xs = if DL.last xs == '/'
                               then DL.filter (\x -> not (DL.elem x ("/" :: String))) xs
                               else xs

--forwardSlashDetectorNM -> This function will
--detect the lack of a forward slash at the
--end of a filepath, and add one if it doesn't (normal).
forwardSlashDetectorNM :: String -> String
forwardSlashDetectorNM [] = []
forwardSlashDetectorNM xs = if DL.last xs /= '/'
                              then xs ++ "/"
                              else xs

--mapNotLast -> This function is a rewrite
--of the map function, but excludes the last
--element of the list.
mapNotLast :: (a -> a) -> [a] -> [a]
mapNotLast fn []     = []
mapNotLast fn [x]    = [x]
mapNotLast fn (x:xs) = fn x : mapNotLast fn xs

{----------------------------}


{-MoveAnnotateMerge Specific Functions.-}

--createFinalDirectory -> This function will
--create the final directories as provided by
--the user.
createFinalDirectory :: [String] -> IO String
createFinalDirectory [] = return []
createFinalDirectory (x:xs) = do
    _ <- SP.readProcess "mkdir" [x] []
    createFinalDirectory xs

--prepareCreateFinalDirectory -> This function will
--prepare the final directorie(s) to be created.
prepareCreateFinalDirectory :: [[String]] -> [String]
prepareCreateFinalDirectory [] = []
prepareCreateFinalDirectory xs = DL.nub (DL.map (DL.!! 2) xs)

--currentFileNameGrabNM -> This function will
--grab the name of the current file in the list (normal).
currentFileNameGrabNM :: String -> String
currentFileNameGrabNM [] = []
currentFileNameGrabNM xs = TR.subRegex (TR.mkRegex ".*\\/") xs ""

--variantsAnnotatedGrabNM -> This function will
--grab the variants.annotated.tsv file from
--readinputfile (normal).
variantsAnnotatedGrabNM :: [String] -> String
variantsAnnotatedGrabNM [] = []
variantsAnnotatedGrabNM xs = xs DL.!! 0

--sampleNameGrab -> This function will
--grab the sample name the the
--variants.annotated.tsv file is associated with.
sampleNameGrab :: [String] -> String
sampleNameGrab [] = []
sampleNameGrab xs = xs DL.!! 1

--lastFinalDirectoryGrabNM -> This function will
--grab the last portion of the final directory.
lastFinalDirectoryGrabNM :: String -> String
lastFinalDirectoryGrabNM [] = []
lastFinalDirectoryGrabNM xs = TR.subRegex (TR.mkRegex ".*\\/") xs ""

--finalDirectoryGrabNM -> This function will
--grab the final directory that the
--variants.annotated.tsv file is associated with (normal).
finalDirectoryGrabNM :: [String] -> String
finalDirectoryGrabNM [] = []
finalDirectoryGrabNM xs = xs DL.!! 2

--variantsAnnotatedSetter -> This function
--will set the variants.annotated.tsv file.
variantsAnnotatedSetter :: String -> [String]   
variantsAnnotatedSetter [] = []         
variantsAnnotatedSetter xs = DL.lines xs

--variantsAnnotatedAnnotator -> This function will
--annotated the variants.annotated.tsv files with
--their corresponding sample names.
variantsAnnotatedAnnotator :: [String] -> [String] -> [String]
variantsAnnotatedAnnotator [] _          = []
variantsAnnotatedAnnotator  _ []         = []
variantsAnnotatedAnnotator xs (y:ys) = [(sampleNameGrab xs) ++ "\t" ++ y] ++ (variantsAnnotatedAnnotator xs ys) 

--variantsAnnotatedMover -> This function will
--copy the files into the correct corresponding
--final directory.
variantsAnnotatedMover :: [[String]] -> Int -> IO String
variantsAnnotatedMover []     _ = return []
variantsAnnotatedMover (x:xs) y = do
    _ <- SP.readProcess "cp" [(variantsAnnotatedGrabNM x)
                             ,(forwardSlashDetectorNM (finalDirectoryGrabNM x))
                           ++ (currentFileNameGrabNM (variantsAnnotatedGrabNM x)) ++ show y] []
    variantsAnnotatedMover xs (y+1)

--variantsAnnotatedPipeline -> This function
--will read the variants.annotated.tsv file
--into a pipeline, and correctly process them.
variantsAnnotatedPipeline :: [[String]] -> Int -> IO ()
variantsAnnotatedPipeline []     _ = return ()
variantsAnnotatedPipeline (x:xs) y = do
    --Read the current file.
    cfile <- SIO.readFile (variantsAnnotatedGrabNM x) 
    --Convert the singular String split by lines.
    let cfileset = variantsAnnotatedSetter cfile
    --Annotate the current file with the current sample name.
    let samplenameadded = variantsAnnotatedAnnotator x cfileset
    --Add newline character to the end of the line.
    let newlinesamplenameadded = [mapNotLast (++ "\n") samplenameadded]
    --Erase contents in the input file.
    SIO.writeFile (forwardSlashDetectorNM (finalDirectoryGrabNM x) ++ currentFileNameGrabNM (variantsAnnotatedGrabNM x) ++ show y) ""
    --Append samplenameadded to ys. 
    SIO.appendFile (forwardSlashDetectorNM (finalDirectoryGrabNM x) ++ currentFileNameGrabNM (variantsAnnotatedGrabNM x) ++ show y) 
                 $ (TPB.render $ 
                   (TPB.hsep 0 TPB.left . DL.map (TPB.vcat TPB.left) . DL.map (DL.map (TPB.text))) 
                   (DL.transpose newlinesamplenameadded))
    --Recurse through the rest of the list.   
    variantsAnnotatedPipeline xs (y+1)

--variantsAnnotatedMerge -> This function will
--merge all files in a single directory.
variantsAnnotatedMerge :: [[String]] -> Int -> IO ()
variantsAnnotatedMerge []     _ = return ()
variantsAnnotatedMerge (x:xs) y = do
    --Read the current file.
    cfile <- SIO.readFile (forwardSlashDetectorNM (finalDirectoryGrabNM x) 
                      ++ currentFileNameGrabNM (variantsAnnotatedGrabNM x) ++ show y)
    --Append cfile to newfile named by the final directory name.
    SIO.appendFile (forwardSlashDetectorNM (finalDirectoryGrabNM x) 
                ++ forwardSlashDeletorNM (lastFinalDirectoryGrabNM (finalDirectoryGrabNM x)) ++ "_merged.tsv") cfile
    --Recurse through the rest of the list.
    variantsAnnotatedMerge xs (y+1)

{---------------------------------------}

{-
{-Main function.-}

main :: IO ()
main = do
    --Get command line arguments.
    cmdargs <- SE.getArgs
    case cmdargs of
        []                   -> error "No argument provided.  Please provide the path to a single .tsv file."
        [inputfile] -> do 
                                --Read inputfile.
                                readinputfile <- SIO.readFile inputfile
                               
                                --Chunk the file.
                                let filechunks = (fileChunker
                                                 (DL.words readinputfile)) 
 
                                --Prepare to create the final directorie(s).
                                let prefinaldirectory = prepareCreateFinalDirectory filechunks                               

                                --Run IO functions.
                                createFinalDirectory prefinaldirectory
                                variantsAnnotatedMover filechunks 1
                                variantsAnnotatedPipeline filechunks 1    
                                variantsAnnotatedMerge filechunks 1
 
        _                    -> error "Too many arguments provided. Please provide the path to a single .tsv file."      

{----------------} 
-}
