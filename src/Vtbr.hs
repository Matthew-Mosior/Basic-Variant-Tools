{-=VariantToBamReadcount (vtbr): A Haskell-based solution=-}
{-=for creating mgibed (bam-readcount input)=-} 
{-=files from transformed ensembl-vep output.=-}
{-=Author: Matthew Mosior=-}
{-=Version: 1.0=-}
{-=Synopsis:  This Haskell Script will take in=-} 
{-=a transformed .vep file and will create a=-}
{-=mgibed file.=-}

{-Module-}

module Vtbr where

{--------}

{-Imports-}

import Data.List as DL
import Data.List.Extra as DLE
import Data.List.Split as DLS
import Data.Ord as DO
import Data.Tuple as DT
import System.Console.GetOpt as SCG
import System.Directory as SD
import System.Environment as SE
import System.Exit as SX
import System.IO as SIO
import System.IO.Temp as SIOT
import System.Process as SP

{---------} 

{-compilerOpts-related functions-}

--checkInFormat -> This function will
--check the format of IN string.
checkInFormat :: String -> Bool
checkInFormat [] = False
checkInFormat xs = if xs == "vcf" || xs == "vep"
                   || xs == "tvcf" || xs == "tvep"
                       then True
                       else False

--checkOutFormat -> This function will
--check the format of OUT string.
checkOutFormat :: String -> Bool
checkOutFormat [] = False
checkOutFormat xs = if xs == "mgibed"
                        then True
                        else False

--checkInOutFormats -> This function will
--check the formats of the IN and OUT string.
checkInOutFormats :: String -> String -> Bool
checkInOutFormats [] [] = False
checkInOutFormats [] _  = False
checkInOutFormats _  [] = False
checkInOutFormats xs ys = if (xs == "vep" && ys == "mgibed")
                          || (xs == "tvep" && ys == "mgibed") 
                          || (xs == "vcf" && ys == "mgibed")
                          || (xs == "tvcf" && ys == "mgibed")
                              then True 
                              else False

{--------------------}

{-Shared General Utility Functions.-}

--lineFeed -> This function will
--read the file in and split on
--whitespace, returning a list
--of lists.
lineFeed :: String -> [[String]]
lineFeed [] = []
lineFeed xs = DL.map DL.words (DL.lines xs)

--mapNotLast -> This function will
--work like the traditional map
--function in Data.List, but not
--map to the last element of a list.
mapNotLast :: (a -> a) -> [a] -> [a]
mapNotLast fn []     = []
mapNotLast fn [x]    = [x]
mapNotLast fn (x:xs) = fn x : mapNotLast fn xs

--tuplifyTwo -> This function will
--turn a list of two elements into
--a two-tuple.
tuplifyTwo :: [a] -> (a,a)
tuplifyTwo [x,y] = (x,y)

--tuplifyThree -> This function will
--turn a list of three elements into
--a triplet.
tuplifyThree :: [a] -> (a,a,a)
tuplifyThree [x,y,z] = (x,y,z)

--singleunnest -> This function will
--unnest a list.
singleunnest :: [a] -> a
singleunnest [a] = a

{-----------------------------------}

{-General Utility Functions (vep/tvep).-}

--onlyDataVepBool -> This function will
--return True for only lines of the 
--file that contain tab-delimited 
--information.
onlyDataVepBool :: [String] -> Bool
onlyDataVepBool xs = not (DL.head xs == "##"
                       || DL.head xs == "#Uploaded_variation")

--onlyDataTvepBool -> This function will
--return True for only lines of the
--file that contain tab-delimited
--information.
onlyDataTvepBool :: [String] -> Bool
onlyDataTvepBool xs = not (DL.head xs == "##"
       || DL.any (\x -> x == "#Uploaded_variation") xs)

--onlyPoundSignBool -> This function will
--return True for only lines of the 
--file that contains the initial 
--header lines.
onlyPoundSignBool :: [String] -> Bool
onlyPoundSignBool xs = DL.head xs == "##"
                    || DL.head xs == "#"

--onlyDataVepGrabber -> This function will 
--grab only lines of the file that 
--contain tab-delimited information.
onlyDataVepGrabber :: [[String]] -> [[String]]
onlyDataVepGrabber [] = []
onlyDataVepGrabber xs = DL.filter (onlyDataVepBool) xs

--onlyDataTvepGrabber -> This function will
--grab only lines of the file that
--contain tab-delimited information.
onlyDataTvepGrabber :: [[String]] -> [[String]]
onlyDataTvepGrabber [] = []
onlyDataTvepGrabber xs = DL.filter (onlyDataTvepBool) xs

--onlyPoundSignGrabber -> This function will
--grab only lines of file that contain
--the initial header lines.
onlyPoundSignGrabber :: [[String]] -> [[String]]
onlyPoundSignGrabber [] = []
onlyPoundSignGrabber xs = DL.filter (onlyPoundSignBool) xs

--parseUploadedVariation -> This function will
--parse the #Uploaded variation field 
--(field containing information needed).
parseUploadedVariation :: [String] -> [(String,Int,(String,String))]
parseUploadedVariation []     = []
parseUploadedVariation (x:xs) = [(\(a,b,c) -> (a,read b,tuplifyTwo (DLS.splitOn "/" c))) 
                                 (tuplifyThree (DLS.splitOn "_" x))] ++ (parseUploadedVariation xs) 

--bamReadcountFormatVep -> This function will
--decipher the appropriate fields for a typical
--bam-readcount input.
bamReadcountFormatVep :: [(String,Int,(String,String))] -> [(String,String,String,String,String)]
bamReadcountFormatVep [] = []
bamReadcountFormatVep (x:xs) = [smallBamReadcountFormatVep x] ++ (bamReadcountFormatVep xs)
    where
        --Nested function definitions.--
        --smallBamReadcountFormatVep
        smallBamReadcountFormatVep :: (String,Int,(String,String)) -> (String,String,String,String,String)
        smallBamReadcountFormatVep (a,b,(c,d)) = (a,show b,show (detectRefVsAltVep b c d),c,d) 
        --detectRefVsAltVep
        detectRefVsAltVep :: Int -> String -> String -> Int
        detectRefVsAltVep a b c = --Insertion.
                               if b == "-"
                                   then a + 1
                                   --Deletion.
                                   else if c == "-"
                                       then a + (DL.length (b DL.\\ c)) - 1 
                                       --Default.
                                       else a
        --------------------------------

{----------------------------}

{-General utility functions (vcf/tvcf).-}

--onlyInfoBool -> This function will
--return True for only lines that 
--contain the "##INFO" fields.
onlyInfoBool :: [String] -> Bool
onlyInfoBool xs = DL.isInfixOf "##INFO" (DL.head xs) 

--onlyInfoGrabber -> This function will
--grab only lines of file that contain
--the initial header lines.
onlyInfoGrabber :: [[String]] -> [[String]]
onlyInfoGrabber [] = []
onlyInfoGrabber xs = DL.filter (onlyInfoBool) xs

--onlyMetadataBool -> This function will
--return true for only lines that 
--metadata.
onlyMetadataBool :: String -> Bool
onlyMetadataBool xs = DL.isPrefixOf "##" xs

--onlyMetadataGrabber -> This function will
--grab only lines of the file that contain
--all header lines.
onlyMetadataGrabber :: [String] -> [String]
onlyMetadataGrabber [] = []
onlyMetadataGrabber xs = DL.filter (onlyMetadataBool) xs

--onlyDataVcfBool -> This function will
--return True for only lines of the
--file that contain tab-delimited
--information.
onlyDataVcfBool :: [String] -> Bool
onlyDataVcfBool xs = not (DL.any (\x -> DL.isPrefixOf "##" x) xs || DL.any (\x -> DL.isPrefixOf "#" x) xs)

--onlyDataVcfGrabber -> This function will
--grab only lines of the file that
--contain tab-delimited information.
onlyDataVcfGrabber :: [[String]] -> [[String]]
onlyDataVcfGrabber [] = []
onlyDataVcfGrabber xs = DL.filter (onlyDataVcfBool) xs

--bamReadcountFormatVcf -> This function will
--decipher the appropriate fields for a typical
--bam-readcount input.
bamReadcountFormatVcf :: [(String,Int,(String,String))] -> [(String,String,String,String,String)]
bamReadcountFormatVcf [] = []
bamReadcountFormatVcf (x:xs) = [smallBamReadcountFormatVcf x] ++ (bamReadcountFormatVcf xs)
    where
        --Nested function definitions.--
        --smallBamReadcountFormatVcf
        smallBamReadcountFormatVcf :: (String,Int,(String,String)) -> (String,String,String,String,String)
        smallBamReadcountFormatVcf (a,b,(c,d)) = (a,
                                                  show (b + 1),
                                                  show ((\(aa,bb,cc) -> aa) (detectRefVsAltVcf (b + 1) c d)),
                                                  (\(aa,bb,cc) -> bb) (detectRefVsAltVcf (b + 1) c d),
                                                  (\(aa,bb,cc) -> cc) (detectRefVsAltVcf (b + 1) c d))
        --detectRefVsAltVcf
        detectRefVsAltVcf :: Int -> String -> String -> (Int,String,String)
        detectRefVsAltVcf a b c = --Insertion.
                               if DL.length b < DL.length c 
                                   then (a + 1,"-",DL.tail c)
                                   --Deletion.
                                   else if DL.length b > DL.length c
                                       then (a + (DL.length (b DL.\\ c)) - 1,DL.tail b,"-")
                                       --Default.
                                       else (a,b,c)

{----------------------------------}

{-Printing functions.-}

--tempFileCreation -> This function will
--print the file to stdout using
--readProcess of the unix tool cat.
catFile :: [[String]] -> IO ()
catFile [] = return ()
catFile xs = do
    --Open a temporary file.
    (tempfile,temph) <- SIOT.openTempFile "." "temp.txt"
    --mapNotLast newlines in xs.
    let intercalatedxs = DL.concat (DL.concat (mapNotLast (++ ["\n"]) xs))
    --Add intercalatedxs to temp.txt.
    hPutStrLn temph intercalatedxs
    --Close the temporary file's handle.
    hClose temph
    --Print out the contents of tempfile to the screen using cat unix tool.
    (_,_,_,ph) <- SP.createProcess (SP.proc "cat" [tempfile])
    ec <- SP.waitForProcess ph
    case ec of
        SX.ExitSuccess   -> do SP.readProcess "rm" [tempfile] []
                               return ()
        SX.ExitFailure _ -> do error "Could not cat file."
                               SP.readProcess "rm" [tempfile] []
                               return ()

--noGzipPrintFile -> This function will
--print the file, not gzipped.
noGzipPrintFile :: String -> [[String]] -> IO ()
noGzipPrintFile [] [] = return ()
noGzipPrintFile [] _  = return ()
noGzipPrintFile _  [] = return ()
noGzipPrintFile outfilestring xs = do
    --mapNotLast newlines in xs.
    let intercalatedxs = DL.concat (DL.concat (mapNotLast (++ ["\n"]) xs))
    --Write the output to the user-specified filename.
    SIO.writeFile outfilestring intercalatedxs
               
--gzipPrintFile -> This function will
--will print the file, but gzipped.
gzipPrintFile :: String -> [[String]] -> IO String
gzipPrintFile [] [] = return []
gzipPrintFile [] _  = return []
gzipPrintFile _  [] = return []
gzipPrintFile outfilestring xs = do
    --mapNotLast newlines in xs.
    let intercalatedxs = DL.concat (DL.concat (mapNotLast (++ ["\n"]) xs))
    --Write the output to the user-specified filename.
    SIO.writeFile outfilestring intercalatedxs
    --Gzip outfile.
    SP.readProcess "gzip" [outfilestring] []

 
{---------------------}

{-VTB Specific Functions.-}

--processArgsAndFilesVepMgibed -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFilesVepMgibed :: (String,String,Bool,Bool,String,FilePath) -> IO ()
processArgsAndFilesVepMgibed ([],[],_,_,[],[])                           = return ()
processArgsAndFilesVepMgibed (inf,outf,gzipin,gzipout,outputf,inputfile) = do
    --Check to see if inputfile is gzip compressed.
    if gzipin
        then do --Decompress the file.
                SP.readProcess "gunzip" [inputfile] []
                --Read the decompressed file.
                gunzippedfile <- SIO.readFile (DLE.dropSuffix ".gz" inputfile)
                --Apply lineFeed function to gunzippedfile.
                let gprocessedfile = lineFeed gunzippedfile
                --Grab only data lines of gprocessedfile.
                let gdataonly = onlyDataVepGrabber gprocessedfile
                --Grab only the #Uploaded_variation column from gdataonly.
                let guploadedvariation = DL.map (DL.head) gdataonly
                --Keep only elements that start with "chr" in guploadedvariation.
                let gchronly = DL.filter (\x -> DL.isPrefixOf "chr" x) guploadedvariation
                --Begin to parse gchronly.
                let gparsedchronly = parseUploadedVariation gchronly
                --Put gparsedchronly into bam-readcount input format.
                let ginitialbamreadcount = bamReadcountFormatVep gparsedchronly
                --Turn ginitialbamreadcount into a list of lists.
                let gfinalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) ginitialbamreadcount
                --mapNotLast tabs to gfinalbamreadcount.
                let gprintbamreadcount = DL.map (mapNotLast (++ "\t")) gfinalbamreadcount
                --Print the file to stdout (cat) or to a file.
                if outputf /= "NONE"
                    --Check to see if outfile is to be gzipped.
                    then if gzipout
                        then do
                            _ <- gzipPrintFile outputf gprintbamreadcount
                            return ()
                        else noGzipPrintFile outputf gprintbamreadcount
                else catFile gprintbamreadcount
 
        else do --Read in the file.
                readinputfile <- SIO.readFile inputfile
                --Apply lineFeed function to inputfile.
                let processedfile = lineFeed readinputfile
                --Grab only data lines of processedfile.
                let dataonly = onlyDataVepGrabber processedfile
                --Grab only the #Uploaded_variation column from dataonly.
                let uploadedvariation = DL.map (DL.head) dataonly
                --Keep only elements that start with "chr" in uploadedvariation.
                let chronly = DL.filter (\x -> DL.isPrefixOf "chr" x) uploadedvariation
                --Begin to parse chronly.
                let parsedchronly = parseUploadedVariation chronly
                --Put parsedchronly into bam-readcount input format.
                let initialbamreadcount = bamReadcountFormatVep parsedchronly
                --Turn initialbamreadcount into a list of lists.
                let finalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) initialbamreadcount
                --mapNotLast tabs to finalbamreadcount.
                let printbamreadcount = DL.map (mapNotLast (++ "\t")) finalbamreadcount
                --Print the file to stdout (cat) or to a file.
                if outputf /= "NONE"
                    --Check to see if outfile is to be gzipped.
                    then if gzipout
                        then do
                            _ <- gzipPrintFile outputf printbamreadcount
                            return ()
                        else noGzipPrintFile outputf printbamreadcount
                else catFile printbamreadcount

--processArgsAndContentsVepMgibed -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContentsVepMgibed :: (String,String,Bool,Bool,String,String) -> IO ()
processArgsAndContentsVepMgibed ([],[],_,_,[],[])                         = return ()
processArgsAndContentsVepMgibed (inf,outf,gzipin,gzipout,outputf,content) = do
    --Apply lineFeed function to content.
    let processedfile = lineFeed content
    --Grab only data lines of processedfile.
    let dataonly = onlyDataVepGrabber processedfile
    --Grab only the #Uploaded_variation column from dataonly.
    let uploadedvariation = DL.map (DL.head) dataonly
    --Keep only elements that start with "chr" in uploadedvariation.
    let chronly = DL.filter (\x -> DL.isPrefixOf "chr" x) uploadedvariation
    --Begin to parse chronly.
    let parsedchronly = parseUploadedVariation chronly
    --Put parsedchronly into bam-readcount input format.
    let initialbamreadcount = bamReadcountFormatVep parsedchronly
    --Turn initialbamreadcount into a list of lists.
    let finalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) initialbamreadcount
    --mapNotLast tabs to finalbamreadcount.
    let printbamreadcount = DL.map (mapNotLast (++ "\t")) finalbamreadcount
    --Print the file to stdout (cat) or to a file.
    if outputf /= "NONE"
        --Check to see if outfile is to be gzipped.
        then if gzipout
            then do
                _ <- gzipPrintFile outputf printbamreadcount
                return ()
            else noGzipPrintFile outputf printbamreadcount
    else catFile printbamreadcount

--processArgsAndFilesTvepMgibed -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFilesTvepMgibed :: (String,String,Bool,Bool,String,FilePath) -> IO ()
processArgsAndFilesTvepMgibed ([],[],_,_,[],[])                           = return ()
processArgsAndFilesTvepMgibed (inf,outf,gzipin,gzipout,outputf,inputfile) = do
    --Check to see if inputfile is gzip compressed.
    if gzipin
        then do --Decompress the file.
                SP.readProcess "gunzip" [inputfile] []
                --Read the decompressed file.
                gunzippedfile <- SIO.readFile (DLE.dropSuffix ".gz" inputfile)
                --Apply lineFeed function to gunzippedfile.
                let gprocessedfile = lineFeed gunzippedfile
                --Grab the index of the #Uploaded_variation column.
                let guploadedvariationindex = singleunnest (DL.nub (DL.concat (DL.map (\x -> DL.elemIndices "#Uploaded_variation" x) gprocessedfile)))
                --Grab only data lines of gprocessedfile.
                let gdataonly = onlyDataTvepGrabber gprocessedfile
                --Grab only the #Uploaded_variation column from gdataonly.
                let guploadedvariation = DL.map (\x -> x DL.!! (guploadedvariationindex)) gdataonly
                --Keep only elements that start with "chr" in guploadedvariation.
                let gchronly = DL.filter (\x -> DL.isPrefixOf "chr" x) guploadedvariation
                --Begin to parse gchronly.
                let gparsedchronly = parseUploadedVariation gchronly
                --Put gparsedchronly into bam-readcount input format.
                let ginitialbamreadcount = bamReadcountFormatVep gparsedchronly
                --Turn ginitialbamreadcount into a list of lists.
                let gfinalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) ginitialbamreadcount
                --mapNotLast tabs to gfinalbamreadcount.
                let gprintbamreadcount = DL.map (mapNotLast (++ "\t")) gfinalbamreadcount
                --Print the file to stdout (cat) or to a file.
                if outputf /= "NONE"
                    --Check to see if outfile is to be gzipped.
                    then if gzipout
                        then do
                            _ <- gzipPrintFile outputf gprintbamreadcount
                            return ()
                        else noGzipPrintFile outputf gprintbamreadcount
                else catFile gprintbamreadcount                

        else do --Read in the file.
                readinputfile <- SIO.readFile inputfile
                --Apply lineFeed function to inputfile.
                let processedfile = lineFeed readinputfile
                --Grab the index of the #Uploaded_variation column.
                let uploadedvariationindex = singleunnest (DL.nub (DL.concat (DL.map (\x -> DL.elemIndices "#Uploaded_variation" x) processedfile)))
                --Grab only data lines of processedfile.
                let dataonly = onlyDataTvepGrabber processedfile
                --Grab only the #Uploaded_variation column from dataonly.
                let uploadedvariation = DL.map (\x -> x DL.!! (uploadedvariationindex)) dataonly
                --Keep only elements that start with "chr" in uploadedvariation.
                let chronly = DL.filter (\x -> DL.isPrefixOf "chr" x) uploadedvariation
                --Begin to parse chronly.
                let parsedchronly = parseUploadedVariation chronly
                --Put parsedchronly into bam-readcount input format.
                let initialbamreadcount = bamReadcountFormatVep parsedchronly
                --Turn initialbamreadcount into a list of lists.
                let finalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) initialbamreadcount
                --mapNotLast tabs to finalbamreadcount.
                let printbamreadcount = DL.map (mapNotLast (++ "\t")) finalbamreadcount
                --Print the file to stdout (cat) or to a file.
                if outputf /= "NONE"
                    --Check to see if outfile is to be gzipped.
                    then if gzipout
                        then do
                            _ <- gzipPrintFile outputf printbamreadcount
                            return ()
                        else noGzipPrintFile outputf printbamreadcount
                else catFile printbamreadcount
           
--processArgsAndContentsTvepMgibed -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContentsTvepMgibed :: (String,String,Bool,Bool,String,String) -> IO ()
processArgsAndContentsTvepMgibed ([],[],_,_,[],[])                         = return ()
processArgsAndContentsTvepMgibed (inf,outf,gzipin,gzipout,outputf,content) = do
    --Apply lineFeed function to content.
    let processedfile = lineFeed content
    --Grab the index of the #Uploaded_variation column.
    let uploadedvariationindex = singleunnest (DL.nub (DL.concat (DL.map (\x -> DL.elemIndices "#Uploaded_variation" x) processedfile)))
    --Grab only data lines of processedfile.
    let dataonly = onlyDataTvepGrabber processedfile
    --Grab only the #Uploaded_variation column from dataonly.
    let uploadedvariation = DL.map (\x -> x DL.!! (uploadedvariationindex)) dataonly
    --Keep only elements that start with "chr" in uploadedvariation.
    let chronly = DL.filter (\x -> DL.isPrefixOf "chr" x) uploadedvariation
    --Begin to parse chronly.
    let parsedchronly = parseUploadedVariation chronly
    --Put parsedchronly into bam-readcount input format.
    let initialbamreadcount = bamReadcountFormatVep parsedchronly
    --Turn initialbamreadcount into a list of lists.
    let finalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) initialbamreadcount
    --mapNotLast tabs to finalbamreadcount.
    let printbamreadcount = DL.map (mapNotLast (++ "\t")) finalbamreadcount
    --Print the file to stdout (cat) or to a file.
    if outputf /= "NONE"
        --Check to see if outfile is to be gzipped.
        then if gzipout
            then do
                _ <- gzipPrintFile outputf printbamreadcount
                return ()
            else noGzipPrintFile outputf printbamreadcount
    else catFile printbamreadcount

--processArgsAndFilesVcfMgibed -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFilesVcfMgibed :: (String,String,Bool,Bool,String,FilePath) -> IO ()
processArgsAndFilesVcfMgibed ([],[],_,_,[],[])                           = return ()
processArgsAndFilesVcfMgibed (inf,outf,gzipin,gzipout,outputf,inputfile) = do
    --Check to see if inputfile is gzip compressed.
    if gzipin
        then do --Decompress the file.
                SP.readProcess "gunzip" [inputfile] []
                --Read the decompressed file.
                gunzippedfile <- SIO.readFile (DLE.dropSuffix ".gz" inputfile)
                --Apply lines to gunzippedfile.
                let gprocessedfile = lineFeed gunzippedfile
                --Grab only the data lines in gprocessedfile.
                let gdataonly = onlyDataVcfGrabber gprocessedfile 
                --Grab only chr, pos, ref and alt columns of gdataonly.
                let gparseddataonly = DL.map (\x -> (x DL.!! 0,read (x DL.!! 1) :: Int,(x DL.!! 3,x DL.!! 4))) gdataonly
                --Put gparseddataonly into bam-readcount input format.
                let ginitialbamreadcount = bamReadcountFormatVcf gparseddataonly
                --Turn ginitialbamreadcount into a list of lists.
                let gfinalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) ginitialbamreadcount
                --mapNotLast tabs to gfinalbamreadcount.
                let gprintbamreadcount = DL.map (mapNotLast (++ "\t")) gfinalbamreadcount
                --Print the file to stdout (cat) or to a file.
                if outputf /= "NONE"
                    --Check to see if outfile is to be gzipped.
                    then if gzipout
                        then do
                            _ <- gzipPrintFile outputf gprintbamreadcount
                            return ()
                        else noGzipPrintFile outputf gprintbamreadcount
                else catFile gprintbamreadcount

        else do --Read in the file.
                readinputfile <- SIO.readFile inputfile
                --Apply lines to readinputfile.
                let processedfile = lineFeed readinputfile
                --Grab only the data lines in processedfile.
                let dataonly = onlyDataVcfGrabber processedfile
                --Grab only chr, pos, ref and alt columns of dataonly.
                let parseddataonly = DL.map (\x -> (x DL.!! 0,read (x DL.!! 1) :: Int,(x DL.!! 3,x DL.!! 4))) dataonly
                --Put parseddataonly into bam-readcount input format.
                let initialbamreadcount = bamReadcountFormatVcf parseddataonly
                --Turn initialbamreadcount into a list of lists.
                let finalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) initialbamreadcount
                --mapNotLast tabs to finalbamreadcount.
                let printbamreadcount = DL.map (mapNotLast (++ "\t")) finalbamreadcount
                --Print the file to stdout (cat) or to a file.
                if outputf /= "NONE"
                    --Check to see if outfile is to be gzipped.
                    then if gzipout
                        then do
                            _ <- gzipPrintFile outputf printbamreadcount
                            return ()
                        else noGzipPrintFile outputf printbamreadcount
                else catFile printbamreadcount

--processArgsAndContentsVcfMgibed -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContentsVcfMgibed :: (String,String,Bool,Bool,String,String) -> IO ()
processArgsAndContentsVcfMgibed ([],[],_,_,[],[])                         = return ()
processArgsAndContentsVcfMgibed (inf,outf,gzipin,gzipout,outputf,content) = do
    --Apply lineFeed function to content.
    let processedfile = lineFeed content
    --Grab only the data lines in processedfile.
    let dataonly = onlyDataVcfGrabber processedfile
    --Grab only chr, pos, ref and alt columns of dataonly.
    let parseddataonly = DL.map (\x -> (x DL.!! 0,read (x DL.!! 1) :: Int,(x DL.!! 3,x DL.!! 4))) dataonly
    --Put parseddataonly into bam-readcount input format.
    let initialbamreadcount = bamReadcountFormatVcf parseddataonly
    --Turn initialbamreadcount into a list of lists.
    let finalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) initialbamreadcount
    --mapNotLast tabs to finalbamreadcount.
    let printbamreadcount = DL.map (mapNotLast (++ "\t")) finalbamreadcount
    --Print the file to stdout (cat) or to a file.
    if outputf /= "NONE"
        --Check to see if outfile is to be gzipped.
        then if gzipout
            then do
                _ <- gzipPrintFile outputf printbamreadcount
                return ()
            else noGzipPrintFile outputf printbamreadcount
    else catFile printbamreadcount

--processArgsAndFilesTvcfMgibed -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFilesTvcfMgibed :: (String,String,Bool,Bool,String,FilePath) -> IO ()
processArgsAndFilesTvcfMgibed ([],[],_,_,[],[])                           = return ()
processArgsAndFilesTvcfMgibed (inf,outf,gzipin,gzipout,outputf,inputfile) = do
    --Check to see if inputfile is gzip compressed.
    if gzipin
        then do --Decompress the file.
                SP.readProcess "gunzip" [inputfile] []
                --Read the decompressed file.
                gunzippedfile <- SIO.readFile (DLE.dropSuffix ".gz" inputfile)
                --Apply lines to gunzippedfile.
                let gprocessedfile = lineFeed gunzippedfile
                --Grab only the data lines in gprocessedfile.
                let gdataonly = onlyDataVcfGrabber gprocessedfile
                --Grab only chr, pos, ref and alt columns of gdataonly.
                let gparseddataonly = DL.map (\x -> (x DL.!! 0,read (x DL.!! 1) :: Int,(x DL.!! 3,x DL.!! 4))) gdataonly
                --Put gparseddataonly into bam-readcount input format.
                let ginitialbamreadcount = bamReadcountFormatVcf gparseddataonly
                --Turn ginitialbamreadcount into a list of lists.
                let gfinalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) ginitialbamreadcount
                --mapNotLast tabs to gfinalbamreadcount.
                let gprintbamreadcount = DL.map (mapNotLast (++ "\t")) gfinalbamreadcount
                --Print the file to stdout (cat) or to a file.
                if outputf /= "NONE"
                    --Check to see if outfile is to be gzipped.
                    then if gzipout
                        then do
                            _ <- gzipPrintFile outputf gprintbamreadcount
                            return ()
                        else noGzipPrintFile outputf gprintbamreadcount
                else catFile gprintbamreadcount

        else do --Read in the file.
                readinputfile <- SIO.readFile inputfile
                --Apply lineFeed function to inputfile.
                let processedfile = lineFeed readinputfile
                --Grab only the data lines in processedfile.
                let dataonly = onlyDataVcfGrabber processedfile
                --Grab only chr, pos, ref and alt columns of dataonly.
                let parseddataonly = DL.map (\x -> (x DL.!! 0,read (x DL.!! 1) :: Int,(x DL.!! 3,x DL.!! 4))) dataonly
                --Put parseddataonly into bam-readcount input format.
                let initialbamreadcount = bamReadcountFormatVcf parseddataonly
                --Turn initialbamreadcount into a list of lists.
                let finalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) initialbamreadcount
                --mapNotLast tabs to finalbamreadcount.
                let printbamreadcount = DL.map (mapNotLast (++ "\t")) finalbamreadcount
                --Print the file to stdout (cat) or to a file.
                if outputf /= "NONE"
                    --Check to see if outfile is to be gzipped.
                    then if gzipout
                        then do
                            _ <- gzipPrintFile outputf finalbamreadcount
                            return ()
                        else noGzipPrintFile outputf finalbamreadcount
                else catFile finalbamreadcount
 
--processArgsAndContentsTvcfMgibed -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContentsTvcfMgibed :: (String,String,Bool,Bool,String,String) -> IO ()
processArgsAndContentsTvcfMgibed ([],[],_,_,[],[])                         = return ()
processArgsAndContentsTvcfMgibed (inf,outf,gzipin,gzipout,outputf,content) = do
    --Apply lineFeed function to content.
    let processedfile = lineFeed content
    --Grab only the data lines in processedfile.
    let dataonly = onlyDataVcfGrabber processedfile
    --Grab only chr, pos, ref and alt columns of dataonly.
    let parseddataonly = DL.map (\x -> (x DL.!! 0,read (x DL.!! 1) :: Int,(x DL.!! 3,x DL.!! 4))) dataonly
    --Put parseddataonly into bam-readcount input format.
    let initialbamreadcount = bamReadcountFormatVcf parseddataonly
    --Turn initialbamreadcount into a list of lists.
    let finalbamreadcount = DL.map (\(a,b,c,d,e) -> [a,b,c,d,e]) initialbamreadcount
    --mapNotLast tabs to finalbamreadcount.
    let printbamreadcount = DL.map (mapNotLast (++ "\t")) finalbamreadcount
    --Print the file to stdout (cat) or to a file.
    if outputf /= "NONE"
        --Check to see if outfile is to be gzipped.
        then if gzipout
            then do
                _ <- gzipPrintFile outputf finalbamreadcount
                return ()
            else noGzipPrintFile outputf finalbamreadcount
    else catFile finalbamreadcount

{-------------------------}
