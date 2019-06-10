{-=BasicVariantParser (BVP): A Haskell-based solution=-}
{-=to ensembl-vep ouput file parsing.=-}
{-=Author: Matthew Mosior=-}
{-=Version: 1.0=-}
{-=Synopsis:  This Haskell Script will take in=-} 
{-=a .vcf or .vep file and parse it accordingly.=-}

{-Module-}

module Bvp where

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
checkOutFormat xs = if xs == "tvcf" || xs == "tvep"
                    || xs == "vcf"  || xs == "vep"
                        then True
                        else False

--checkInOutFormats -> This function will
--check the formats of the IN and OUT string.
checkInOutFormats :: String -> String -> Bool
checkInOutFormats [] [] = False
checkInOutFormats [] _  = False
checkInOutFormats _  [] = False
checkInOutFormats xs ys = if (xs == "vep" && ys == "tvep")
                          || (xs == "tvep" && ys == "vep") 
                          || (xs == "vcf" && ys == "tvcf")
                          || (xs == "tvcf" && ys == "vcf")
                              then True 
                              else False

{--------------------}

{-General Utility Functions (Vep).-}

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

--onlyDataVepBool -> This function will
--return True for only lines of the 
--file that contain tab-delimited 
--information.
onlyDataVepBool :: [String] -> Bool
onlyDataVepBool xs = not (DL.head xs == "##")

--onlyPoundSignBool -> This function will
--return True for only lines of the 
--file that contains the initial 
--header lines.
onlyPoundSignBool :: [String] -> Bool
onlyPoundSignBool xs = DL.head xs == "##"

--onlyDataVepGrabber -> This function will 
--grab only lines of the file that 
--contain tab-delimited information.
onlyDataVepGrabber :: [[String]] -> [[String]]
onlyDataVepGrabber [] = []
onlyDataVepGrabber xs = DL.filter (onlyDataVepBool) xs

--onlyPoundSignGrabber -> This function will
--grab only lines of file that contain
--the initial header lines.
onlyPoundSignGrabber :: [[String]] -> [[String]]
onlyPoundSignGrabber [] = []
onlyPoundSignGrabber xs = DL.filter (onlyPoundSignBool) xs

--orderList -> This function will
--order a nested list.
orderList :: [[[String]]] -> [Int] -> [[[(String,Int)]]]
orderList [] [] = []
orderList _  [] = []
orderList [] _  = []
orderList (x:xs) (y:ys) = [DL.map (DL.map (\z -> (z,y))) x] ++ (orderList xs ys)

--tuplifyTwo -> This function will
--turn a list of two elements into
--a two-tuple.
tuplifyTwo :: [a] -> (a,a)
tuplifyTwo [x,y] = (x,y)

--singleunnest -> This function will
--unnest a list.
singleunnest :: [a] -> a
singleunnest [a] = a

--columnMatcher -> This function will
--take data and match it to the 
--appropriate column.
columnMatcher :: [[((String,Int),(String,Int))]] -> [[String]] -> [[[(String,(String,Int))]]]
columnMatcher [] []     = []
columnMatcher [] _      = []
columnMatcher _ []      = []
columnMatcher (x:xs) ys = [smallColumnMatcher x ys] ++ (columnMatcher xs ys)
    where
        --Nested function defintion.--
        smallColumnMatcher :: [((String,Int),(String,Int))] -> [[String]] -> [[(String,(String,Int))]]
        smallColumnMatcher [] []     = []
        smallColumnMatcher [] _      = []
        smallColumnMatcher _ []      = []
        smallColumnMatcher (x:xs) ys = [[((singleunnest (singleunnest (DL.filter (\y -> (fst (fst x)) == (singleunnest y)) ys))),snd x)]] ++ (smallColumnMatcher xs ys)
        ------------------------------

--missingColumnAdder -> This function will
--add back missing values from result of
--the sorted columnMatcher function.
missingColumnAdder :: [[(String,(String,Int))]] -> [String] -> [[(String,(String,Int))]]
missingColumnAdder [] [] = []
missingColumnAdder _  [] = []
missingColumnAdder [] _  = []
missingColumnAdder (x:xs) ys = [smallMissingColumnAdder x ys] ++ (missingColumnAdder xs ys)
    where
        --Nested function definitions.--
        --smallMissingColumnAdder
        smallMissingColumnAdder :: [(String,(String,Int))] -> [String] -> [(String,(String,Int))]
        smallMissingColumnAdder [] []     = []
        smallMissingColumnAdder _  []     = []
        smallMissingColumnAdder [] _      = []
        smallMissingColumnAdder xs ys     = (DL.map (\z -> (z,("N/A",snd (snd (DL.head xs))))) (ys DL.\\ (DL.map (fst) xs))) ++ xs
        -------------------------------- 

--mergeLists -> This function will
--merge lists from two different nested
--lists.
mergeLists :: [[String]] -> [[String]] -> [[String]]
mergeLists [] []         = []
mergeLists [] _          = []
mergeLists _  []         = []
mergeLists (x:xs) (y:ys) = [x ++ y] ++ (mergeLists xs ys)

{----------------------------}

{-General utility functions (vcf).-}

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
onlyDataVcfBool xs = not (DL.isInfixOf "##" (DL.head xs))

--onlyDataVcfGrabber -> This function will
--grab only lines of the file that
--contain tab-delimited information.
onlyDataVcfGrabber :: [[String]] -> [[String]]
onlyDataVcfGrabber [] = []
onlyDataVcfGrabber xs = DL.filter (onlyDataVcfBool) xs

--insertSubfields -> This function will
--insert subfields.
insertSubfields :: [[String]] -> [String] -> [[String]]
insertSubfields []    _  = []
insertSubfields _     [] = []
insertSubfields [x,y] z  = [x,z,y]

--dataReplicator -> This function will
--replicate the data field based on the
--amount of data associated with the "CSQ"
--field.
dataReplicator :: [[String]] -> [Int] -> [[String]]
dataReplicator [] _ = []
dataReplicator _ [] = []
dataReplicator (x:xs) (y:ys) = (DL.replicate y x) ++ (dataReplicator xs ys)

--dataCombinator -> This function will
--replicate the data field based on the
--amount of data associated with the "CSQ"
--field.
dataCombinator :: [[String]] -> [[String]] -> [[String]] -> [[String]]
dataCombinator [] _  [] = []
dataCombinator _  [] [] = []
dataCombinator [] [] _  = []
dataCombinator _  _  [] = []
dataCombinator [] _  _  = []
dataCombinator (x:xs) (y:ys) (z:zs) = [x ++ y ++ z] ++ (dataCombinator xs ys zs)

--notApplicableAdder -> This function will
--add N/A in-place of each null string.
notApplicableAdder :: [[String]] -> [[String]]
notApplicableAdder [] = []
notApplicableAdder (x:xs) = [smallNotApplicableAdder x] ++ (notApplicableAdder xs)
    where
        --Nested function definition.--
        smallNotApplicableAdder :: [String] -> [String] 
        smallNotApplicableAdder [] = []
        smallNotApplicableAdder (x:xs) = if DL.null x
                                             then ["N/A"] ++ (smallNotApplicableAdder xs)
                                             else [x] ++ (smallNotApplicableAdder xs)
        -------------------------------

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

{-BVP Specific Functions.-}

--processArgsAndFilesVepTvep -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFilesVepTvep :: (String,String,Bool,Bool,String,FilePath) -> IO ()
processArgsAndFilesVepTvep ([],[],_,_,[],[]) = return ()
processArgsAndFilesVepTvep (inf,outf,gzipin,gzipout,outputf,inputfile) = do
    --Check to see if inputfile is gzip compressed.
    if gzipin
        then do --Decompress the file.
                SP.readProcess "gunzip" [inputfile] []
                --Read the decompressed file.
                gunzippedfile <- SIO.readFile (DLE.dropSuffix ".gz" inputfile)               
                --Apply lineFeed function to inputfile.
                let gprocessedfile = lineFeed gunzippedfile
                --Grab only the data portion of processedfile.
                let gonlydata = onlyDataVepGrabber gprocessedfile
                --Grab all of onlydata except for the last column.
                let gallbutextra = DL.map (DL.init) gonlydata
                --Grab only the header portion of processedfile.
                let gonlyheaders = onlyPoundSignGrabber gprocessedfile
                --Grab only the last field "Extra" from onlydata.
                let gonlydatalastfield = DL.map (DL.last) gonlydata
                --Split the subfields of onlydatalastfield by semicolon delimiter.
                let gsplitlastfield = DL.map (DLS.splitOn ";") gonlydatalastfield
                --Split each subfield of the subfields of splitlastfield by equal-sign delimiter.
                let gsplitkeyvalue = DL.map (DL.map (DLS.splitOn "=")) gsplitlastfield
                --Order the sublists of splitkeyvalue so that they can be reordered later.
                let gsplitkeyvaluenumbered = orderList gsplitkeyvalue [0..(DL.length gsplitkeyvalue -1)]
                --Grab only the keys of the key-value pair from splitkeyvalue.
                let gkeysonly = DL.map (DL.map (DL.head)) gsplitkeyvalue
                --Grab all keys from keysonly.
                let gallkeys = DL.nub (DL.sort (DL.concat gkeysonly))
                --Nested each of the keys within all keys into a sublist.
                let gallkeysnested = DL.map (\x -> [x]) gallkeys
                --Grab everything but the head of splitkeyvaluenumbered.
                let gsplitkeyvaluesanshead = DL.tail gsplitkeyvaluenumbered
                --Turn list of two items into tuple of splitkeyaluesanshead.
                let gkeyvaluetuple = DL.map (DL.map (tuplifyTwo)) gsplitkeyvaluesanshead
                --Match the columns of keyvaluetuple and allkeysnested.
                let gmatchedcolumns = columnMatcher gkeyvaluetuple gallkeysnested
                --Concatenate the sublists within matchedcolumns
                let gconcatmatchedcolumns = DL.map (DL.concat) gmatchedcolumns
                --Add the missing column values back to each sublist of concatmatchedcolumns.
                let gcolumnsaddedback = missingColumnAdder gconcatmatchedcolumns gallkeys
                --Sort each list of columnsaddedback.
                let gsortedmatchedcolumns = DL.map (DL.sortOn (fst)) gcolumnsaddedback
                --Grab only the values of the columns.
                let gfinalmatchedcolumns = DL.map (DL.map (\(_,(y,_)) -> y)) gsortedmatchedcolumns
                --Add all keys back onto finalmatchedcolumns.
                let gaddcolumnheaders = gallkeys : gfinalmatchedcolumns
                --Merge each sublist of allbutextra and addcolumnheaders.
                let gfinalmergedlists = mergeLists gallbutextra gaddcolumnheaders
                --mapNotLast tabs in finalmergedlists.
                let gfinalmergedliststabbed = DL.map (mapNotLast (++ "\t")) gfinalmergedlists
                --mapNotLast spaces in onlyheaders.
                let gonlyheadersspaces = DL.map (mapNotLast (++ " ")) gonlyheaders
                --Add the initial "##" VEP header section back onto finalmergedlists.
                let gfinaloutput = gonlyheadersspaces ++ gfinalmergedliststabbed
                --Print the file to stdout (cat) or to a file.
                if outputf /= "NONE"
                    --Check to see if outfile is to be gzipped.
                    then if gzipout
                        then do
                            _ <- gzipPrintFile outputf gfinaloutput
                            return ()
                        else noGzipPrintFile outputf gfinaloutput
                else catFile gfinaloutput

        else do --Read in the file.
                readinputfile <- SIO.readFile inputfile
                --Apply lineFeed function to inputfile.
                let processedfile = lineFeed readinputfile
                --Grab only the data portion of processedfile.
                let onlydata = onlyDataVepGrabber processedfile
                --Grab all of onlydata except for the last column.
                let allbutextra = DL.map (DL.init) onlydata
                --Grab only the header portion of processedfile.
                let onlyheaders = onlyPoundSignGrabber processedfile
                --Grab only the last field "Extra" from onlydata.
                let onlydatalastfield = DL.map (DL.last) onlydata
                --Split the subfields of onlydatalastfield by semicolon delimiter.
                let splitlastfield = DL.map (DLS.splitOn ";") onlydatalastfield
                --Split each subfield of the subfields of splitlastfield by equal-sign delimiter.
                let splitkeyvalue = DL.map (DL.map (DLS.splitOn "=")) splitlastfield
                --Order the sublists of splitkeyvalue so that they can be reordered later.
                let splitkeyvaluenumbered = orderList splitkeyvalue [0..(DL.length splitkeyvalue -1)] 
                --Grab only the keys of the key-value pair from splitkeyvalue.
                let keysonly = DL.map (DL.map (DL.head)) splitkeyvalue
                --Grab all keys from keysonly.
                let allkeys = DL.nub (DL.sort (DL.concat keysonly)) 
                --Nested each of the keys within all keys into a sublist.
                let allkeysnested = DL.map (\x -> [x]) allkeys
                --Grab everything but the head of splitkeyvaluenumbered.
                let splitkeyvaluesanshead = DL.tail splitkeyvaluenumbered
                --Turn list of two items into tuple of splitkeyaluesanshead.
                let keyvaluetuple = DL.map (DL.map (tuplifyTwo)) splitkeyvaluesanshead
                --Match the columns of keyvaluetuple and allkeysnested.
                let matchedcolumns = columnMatcher keyvaluetuple allkeysnested
                --Concatenate the sublists within matchedcolumns
                let concatmatchedcolumns = DL.map (DL.concat) matchedcolumns
                --Add the missing column values back to each sublist of concatmatchedcolumns.
                let columnsaddedback = missingColumnAdder concatmatchedcolumns allkeys
                --Sort each list of columnsaddedback.
                let sortedmatchedcolumns = DL.map (DL.sortOn (fst)) columnsaddedback
                --Grab only the values of the columns.
                let finalmatchedcolumns = DL.map (DL.map (\(_,(y,_)) -> y)) sortedmatchedcolumns
                --Add all keys back onto finalmatchedcolumns.
                let addcolumnheaders = allkeys : finalmatchedcolumns
                --Merge each sublist of allbutextra and addcolumnheaders.
                let finalmergedlists = mergeLists allbutextra addcolumnheaders
                --mapNotLast tabs in finalmergedlists.
                let finalmergedliststabbed = DL.map (mapNotLast (++ "\t")) finalmergedlists
                --mapNotLast spaces in onlyheaders.
                let onlyheadersspaces = DL.map (mapNotLast (++ " ")) onlyheaders
                --Add the initial "##" VEP header section back onto finalmergedlists.
                let finaloutput = onlyheadersspaces ++ finalmergedliststabbed
                --Print the file to stdout (cat) or to a file.
                if outputf /= "NONE"
                    --Check to see if outfile is to be gzipped.
                    then if gzipout
                        then do
                            _ <- gzipPrintFile outputf finaloutput
                            return ()
                        else noGzipPrintFile outputf finaloutput
                else catFile finaloutput

--processArgsAndContentsVepTvep -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContentsVepTvep :: (String,String,Bool,Bool,String,String) -> IO ()
processArgsAndContentsVepTvep ([],[],_,_,[],[]) = return ()
processArgsAndContentsVepTvep (inf,outf,gzipin,gzipout,outputf,content) = do
    --Apply lineFeed function to inputfile.
    let processedfile = lineFeed content
    --Grab only the data portion of processedfile.
    let onlydata = onlyDataVepGrabber processedfile
    --Grab all of onlydata except for the last column.
    let allbutextra = DL.map (DL.init) onlydata
    --Grab only the header portion of processedfile.
    let onlyheaders = onlyPoundSignGrabber processedfile
    --Grab only the last field "Extra" from onlydata.
    let onlydatalastfield = DL.map (DL.last) onlydata
    --Split the subfields of onlydatalastfield by semicolon delimiter.
    let splitlastfield = DL.map (DLS.splitOn ";") onlydatalastfield
    --Split each subfield of the subfields of splitlastfield by equal-sign delimiter.
    let splitkeyvalue = DL.map (DL.map (DLS.splitOn "=")) splitlastfield
    --Order the sublists of splitkeyvalue so that they can be reordered later.
    let splitkeyvaluenumbered = orderList splitkeyvalue [0..(DL.length splitkeyvalue -1)]
    --Grab only the keys of the key-value pair from splitkeyvalue.
    let keysonly = DL.map (DL.map (DL.head)) splitkeyvalue
    --Grab all keys from keysonly.
    let allkeys = DL.nub (DL.sort (DL.concat keysonly))
    --Nested each of the keys within all keys into a sublist.
    let allkeysnested = DL.map (\x -> [x]) allkeys
    --Grab everything but the head of splitkeyvaluenumbered.
    let splitkeyvaluesanshead = DL.tail splitkeyvaluenumbered
    --Turn list of two items into tuple of splitkeyaluesanshead.
    let keyvaluetuple = DL.map (DL.map (tuplifyTwo)) splitkeyvaluesanshead
    --Match the columns of keyvaluetuple and allkeysnested.
    let matchedcolumns = columnMatcher keyvaluetuple allkeysnested
    --Concatenate the sublists within matchedcolumns
    let concatmatchedcolumns = DL.map (DL.concat) matchedcolumns
    --Add the missing column values back to each sublist of concatmatchedcolumns.
    let columnsaddedback = missingColumnAdder concatmatchedcolumns allkeys
    --Sort each list of columnsaddedback.
    let sortedmatchedcolumns = DL.map (DL.sortOn (fst)) columnsaddedback
    --Grab only the values of the columns.
    let finalmatchedcolumns = DL.map (DL.map (\(_,(y,_)) -> y)) sortedmatchedcolumns
    --Add all keys back onto finalmatchedcolumns.
    let addcolumnheaders = allkeys : finalmatchedcolumns
    --Merge each sublist of allbutextra and addcolumnheaders.
    let finalmergedlists = mergeLists allbutextra addcolumnheaders
    --mapNotLast tabs in finalmergedlists.
    let finalmergedliststabbed = DL.map (mapNotLast (++ "\t")) finalmergedlists
    --mapNotLast spaces in onlyheaders.
    let onlyheadersspaces = DL.map (mapNotLast (++ " ")) onlyheaders
    --Add the initial "##" VEP header section back onto finalmergedlists.
    let finaloutput = onlyheadersspaces ++ finalmergedliststabbed
    --Print the file to stdout (cat) or to a file.
    if outputf /= "NONE"
        --Check to see if outfile is to be gzipped.
        then if gzipout
            then do
                _ <- gzipPrintFile outputf finaloutput
                return ()
            else noGzipPrintFile outputf finaloutput
    else catFile finaloutput

--processArgsAndFilesTvepVep -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFilesTvepVep :: (String,String,Bool,Bool,String,FilePath) -> IO ()
processArgsAndFilesTvepVep ([],[],_,_,[],[]) = return ()
processArgsAndFilesTvepVep (inf,outf,gzipin,gzipout,outputf,inputfile) = do
    --Read in the file.
    readinputfile <- SIO.readFile inputfile
    print "tvepvepfiles"

--processArgsAndContentsTvepVep -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContentsTvepVep :: (String,String,Bool,Bool,String,String) -> IO ()
processArgsAndContentsTvepVep ([],[],_,_,[],[]) = return ()
processArgsAndContentsTvepVep (inf,outf,gzipin,gzipout,outputf,content) = do
    --Apply lineFeed function to inputfile.
    let processedfile = lineFeed content
    print "tvepvepcontents"

--processArgsAndFilesVcfTvcf -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFilesVcfTvcf :: (String,String,Bool,Bool,String,FilePath) -> IO ()
processArgsAndFilesVcfTvcf ([],[],_,_,[],[]) = return ()
processArgsAndFilesVcfTvcf (inf,outf,gzipin,gzipout,outputf,inputfile) = do 
    --Check to see if inputfile is gzip compressed.
    if gzipin
        then do --Decompress the file.
                SP.readProcess "gunzip" [inputfile] []
                --Read the decompressed file.
                gunzippedfile <- SIO.readFile (DLE.dropSuffix ".gz" inputfile)
                --Apply lineFeed function to gunzippedfile.
                let gprocessedfile = lineFeed gunzippedfile
                --Apply lines to gunzippedfile.
                let gprocessedfilenew = DL.lines gunzippedfile
                --Grab only the INFO portion of processedfile.
                let gonlyinfo = onlyInfoGrabber gprocessedfile
                --Grab only the data lines in gprocessedfile.
                let gdataonly = onlyDataVcfGrabber gprocessedfile
                --Grab all metadata lines from gprocessedfile.
                let gallmetadata = onlyMetadataGrabber gprocessedfilenew
                --Grab only the INFO Field of gdataonly.
                let ginfodataonly = DL.map (\x -> [x DL.!! 7]) (DL.tail gdataonly) 
                --Grab all head fields except for the INFO field of gdataonly.
                let gheadinfodataonly = DL.map (DL.take 6) (DL.tail gdataonly)
                --Grab all tail fields except for the INFO field of gdataonly.
                let gtailinfodataonly = DL.map (DL.drop 8) (DL.tail gdataonly)
                --Split the subfields of gdataonly by semicolon delimiter.
                let gsplitsemicolon = DL.map (DL.map (DLS.splitOn ";")) ginfodataonly
                --Split each subfield of the subfields of gsplitsemicolon by equal-sign delimiter.
                let gsplitequals = DL.map (DL.map (DL.map (DLS.splitOn "="))) gsplitsemicolon
                --Grab only the list with "CSQ" as the first (head) element.
                let gcsqsplitequals = DL.map (DL.map (DL.filter (\x -> DL.head x == "CSQ"))) gsplitequals
                --Grab only the last element of gheadsplitequals
                let glastcsq = DL.map (DL.last) gcsqsplitequals
                --Split each subfield of the sublists of gsplitequals by comma delimiter.
                let gsplitcomma = DL.map (DL.map (DL.map (DLS.splitOn ","))) glastcsq
                --Grab the last field of gsplitcomma.
                let glastsplitcomma = DL.map (DL.map (DL.last)) gsplitcomma
                --Split each item in glastsplitcomma by | delimiter.
                let gsplitlastsplitcomma = DL.map (DL.map (DL.map (DLS.splitOn "|"))) glastsplitcomma
                --Grab just the headers for the fields of gonlyinfo.
                let gheadersonlyinfo = DL.map (DL.last) (DL.map (DLS.splitOn "=") (DL.map (DL.head) (DL.map (DLS.splitOn ",") (DL.map (DL.head) gonlyinfo))))
                --Grab just the CSQ header from gonlyinfo.
                let gcsqheadersonlyinfo = DL.concat (DL.concat (DL.filter (DL.any (\x -> DL.isInfixOf "##INFO=<ID=CSQ" x)) gonlyinfo))
                --Grab the subfields from gcsqheadersonlyinfo.
                let gsubfieldscsqheadersonlyinfo = DLS.splitOn "|" (DL.filter (\x -> x /= '"' && x /= '>') (DL.last (DLS.splitOn ":" gcsqheadersonlyinfo)))
                --Remove "CSQ" from gheadersonlyinfo.
                let gfinalheadersonlyinfo =  DLS.splitWhen (\x -> x == "CSQ") gheadersonlyinfo
                --Insert gsubfieldscsqheadersonlyinfo into gfinalheadersonlyinfo.
                let gfinalheaders = DL.concat (insertSubfields gfinalheadersonlyinfo gsubfieldscsqheadersonlyinfo)
                --Grab data header from gdataonly.
                let gdataheader = DL.concat (DL.filter (\x -> DL.head x == "#CHROM") gdataonly)
                --Remove "INFO" from gdataheader.
                let gfinaldataheader = DLS.splitWhen (\x -> x == "INFO") gdataheader
                --Insert finalheaders into finaldataheader.
                let gtruefinalheader = DL.concat (insertSubfields gfinaldataheader gfinalheaders)
                --Remove CSQ from gtruefinalheader.
                let gactualtruefinalheader = DLS.splitWhen (\x -> x == "CSQ") gtruefinalheader
                --Relicate gheadinfodataonly the correct number of times.
                let gheadreplicateddata = dataReplicator gheadinfodataonly (DL.concat (DL.map (DL.map (DL.length)) gsplitlastsplitcomma))
                --Replicate gtailinfodataonly the correct number of times.
                let gtailreplicateddata = dataReplicator gtailinfodataonly (DL.concat (DL.map (DL.map (DL.length)) gsplitlastsplitcomma)) 
                --Concatenate gsplitlastsplitcomma.
                let gconcatsplitlastsplitcomma = (DL.concat (DL.concat gsplitlastsplitcomma))
                --Combined greplicateddata and gsplitlastsplitcomma.
                let gfinalizeddata = dataCombinator gheadreplicateddata gconcatsplitlastsplitcomma gtailreplicateddata
                --Add gallmetadata to gactualtruefinalheader and gfinalizeddata.
                let gfinalfinaldata = [mapNotLast (++ "\n") gallmetadata] ++ (DL.map (mapNotLast (++ "\t")) gactualtruefinalheader) 
                                                     ++ (DL.map (mapNotLast (++ "\t")) (notApplicableAdder gfinalizeddata))
                --Print the file to stdout (cat) or to a file.
                if outputf /= "NONE"
                    --Check to see if outfile is to be gzipped.
                    then if gzipout
                        then do
                            _ <- gzipPrintFile outputf gfinalfinaldata
                            return ()
                        else noGzipPrintFile outputf gfinalfinaldata
                else catFile gfinalfinaldata
               
        else do --Read in the file.
                readinputfile <- SIO.readFile inputfile
                --Apply lineFeed function to inputfile.
                let processedfile = lineFeed readinputfile
                --Apply lines to inputfile.
                let processedfilenew = DL.lines readinputfile
                --Grab only the data portion of processedfile.
                let onlyinfo = onlyInfoGrabber processedfile
                --Grab only the data lines in processedfile.
                let dataonly = onlyDataVcfGrabber processedfile
                --Grab all metadata lines from processedfile.
                let allmetadata = onlyMetadataGrabber processedfilenew
                --Grab only the INFO field of dataonly.
                let infodataonly = DL.map (\x -> [x DL.!! 7]) (DL.tail dataonly)
                --Grab all head fields except for the INFO field of dataonly.
                let headinfodataonly = DL.map (DL.take 6) (DL.tail dataonly)
                --Grab all tail fields except for the INFO field of dataonly.
                let tailinfodataonly = DL.map (DL.drop 8) (DL.tail dataonly)
                --Split the subfields of gdataonly by semicolon delimiter.
                let splitsemicolon = DL.map (DL.map (DLS.splitOn ";")) infodataonly
                --Split each subfield of the subfields of splitsemicolon by equal-sign delimiter.
                let splitequals = DL.map (DL.map (DL.map (DLS.splitOn "="))) splitsemicolon
                --Grab only the list with "CSQ" as the first (head) element.
                let csqsplitequals = DL.map (DL.map (DL.filter (\x -> DL.head x == "CSQ"))) splitequals
                --Grab only the last element of csqsplitequals.
                let lastcsq = DL.map (DL.last) csqsplitequals
                --Split each subfield of the sublists of splitequals by comma delimiter.
                let splitcomma = DL.map (DL.map (DL.map (DLS.splitOn ","))) lastcsq
                --Grab the last field of splitcomma.
                let lastsplitcomma = DL.map (DL.map (DL.last)) splitcomma 
                --Split each item in lastsplitcomma by | delimiter.
                let splitlastsplitcomma = DL.map (DL.map (DL.map (DLS.splitOn "|"))) lastsplitcomma
                --Grab just the headers for the fields of onlyinfo.
                let headersonlyinfo = DL.map (DL.last) (DL.map (DLS.splitOn "=") (DL.map (DL.head) (DL.map (DLS.splitOn ",") (DL.map (DL.head) onlyinfo))))
                --Grab just the CSQ header from onlyinfo.
                let csqheadersonlyinfo = DL.concat (DL.concat (DL.filter (DL.any (\x -> DL.isInfixOf "##INFO=<ID=CSQ" x)) onlyinfo))
                --Grab the subfields from csqheadersonlyinfo.
                let subfieldscsqheadersonlyinfo = DLS.splitOn "|" (DL.filter (\x -> x /= '"' && x /= '>') (DL.last (DLS.splitOn ":" csqheadersonlyinfo)))
                --Remove "CSQ" from headersonlyinfo.
                let finalheadersonlyinfo =  DLS.splitWhen (\x -> x == "CSQ") headersonlyinfo
                --Insert subfieldscsqheadersonlyinfo into finalheadersonlyinfo.
                let finalheaders = DL.concat (insertSubfields finalheadersonlyinfo subfieldscsqheadersonlyinfo)
                --Grab data header from dataonly.
                let dataheader = DL.concat (DL.filter (\x -> DL.head x == "#CHROM") dataonly)
                --Remove "INFO" from dataheader.
                let finaldataheader = DLS.splitWhen (\x -> x == "INFO") dataheader
                --Insert finalheaders into finaldataheader.
                let truefinalheader = DL.concat (insertSubfields finaldataheader finalheaders) 
                --Remove CSQ from truefinalheader.
                let actualtruefinalheader = DLS.splitWhen (\x -> x == "CSQ") truefinalheader
                --Relicate headinfodataonly the correct number of times.
                let headreplicateddata = dataReplicator headinfodataonly (DL.concat (DL.map (DL.map (DL.length)) splitlastsplitcomma))
                --Replicate tailinfodataonly the correct number of times.
                let tailreplicateddata = dataReplicator tailinfodataonly (DL.concat (DL.map (DL.map (DL.length)) splitlastsplitcomma))
                --Concatenate splitlastsplitcomma.
                let concatsplitlastsplitcomma = (DL.concat (DL.concat splitlastsplitcomma))
                --Combined replicateddata and splitlastsplitcomma.
                let finalizeddata = dataCombinator headreplicateddata concatsplitlastsplitcomma tailreplicateddata
                --Add allmetadata to actualtruefinalheader and finalizeddata.
                let finalfinaldata = [mapNotLast (++ "\n") allmetadata] ++ (DL.map (mapNotLast (++ "\t")) actualtruefinalheader) 
                                                   ++ (DL.map (mapNotLast (++ "\t")) (notApplicableAdder finalizeddata))
                --Print the file to stdout (cat) or to a file.
                if outputf /= "NONE"
                    --Check to see if outfile is to be gzipped.
                    then if gzipout
                        then do
                            _ <- gzipPrintFile outputf finalfinaldata
                            return ()
                        else noGzipPrintFile outputf finalfinaldata
                else catFile finalfinaldata
 
--processArgsAndContentsVcfTvcf -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContentsVcfTvcf :: (String,String,Bool,Bool,String,String) -> IO ()
processArgsAndContentsVcfTvcf ([],[],_,_,[],[]) = return ()
processArgsAndContentsVcfTvcf (inf,outf,gzipin,gzipout,outputf,content) = do
    --Apply lineFeed function to content.
    let processedfile = lineFeed content
    --Apply lines to inputfile.
    let processedfilenew = DL.lines content
    --Grab only the data portion of processedfile.
    let onlyinfo = onlyInfoGrabber processedfile
    --Grab only the data lines in processedfile.
    let dataonly = onlyDataVcfGrabber processedfile
    --Grab all metadata lines from processedfile.
    let allmetadata = onlyMetadataGrabber processedfilenew
    --Grab only the INFO field of dataonly.
    let infodataonly = DL.map (\x -> [x DL.!! 7]) (DL.tail dataonly)
    --Grab all head fields except for the INFO field of dataonly.
    let headinfodataonly = DL.map (DL.take 6) (DL.tail dataonly)
    --Grab all tail fields except for the INFO field of dataonly.
    let tailinfodataonly = DL.map (DL.drop 8) (DL.tail dataonly)
    --Split the subfields of gdataonly by semicolon delimiter.
    let splitsemicolon = DL.map (DL.map (DLS.splitOn ";")) infodataonly
    --Split each subfield of the subfields of splitsemicolon by equal-sign delimiter.
    let splitequals = DL.map (DL.map (DL.map (DLS.splitOn "="))) splitsemicolon
    --Grab only the list with "CSQ" as the first (head) element.
    let csqsplitequals = DL.map (DL.map (DL.filter (\x -> DL.head x == "CSQ"))) splitequals
    --Grab only the last element of csqsplitequals.
    let lastcsq = DL.map (DL.last) csqsplitequals
    --Split each subfield of the sublists of splitequals by comma delimiter.
    let splitcomma = DL.map (DL.map (DL.map (DLS.splitOn ","))) lastcsq
    --Grab the last field of splitcomma.
    let lastsplitcomma = DL.map (DL.map (DL.last)) splitcomma
    --Split each item in lastsplitcomma by | delimiter.
    let splitlastsplitcomma = DL.map (DL.map (DL.map (DLS.splitOn "|"))) lastsplitcomma
    --Grab just the headers for the fields of onlyinfo.
    let headersonlyinfo = DL.map (DL.last) (DL.map (DLS.splitOn "=") (DL.map (DL.head) (DL.map (DLS.splitOn ",") (DL.map (DL.head) onlyinfo))))
    --Grab just the CSQ header from onlyinfo.
    let csqheadersonlyinfo = DL.concat (DL.concat (DL.filter (DL.any (\x -> DL.isInfixOf "##INFO=<ID=CSQ" x)) onlyinfo))
    --Grab the subfields from csqheadersonlyinfo.
    let subfieldscsqheadersonlyinfo = DLS.splitOn "|" (DL.filter (\x -> x /= '"' && x /= '>') (DL.last (DLS.splitOn ":" csqheadersonlyinfo)))
    --Remove "CSQ" from headersonlyinfo.
    let finalheadersonlyinfo =  DLS.splitWhen (\x -> x == "CSQ") headersonlyinfo
    --Insert subfieldscsqheadersonlyinfo into finalheadersonlyinfo.
    let finalheaders = DL.concat (insertSubfields finalheadersonlyinfo subfieldscsqheadersonlyinfo)
    --Grab data header from dataonly.
    let dataheader = DL.concat (DL.filter (\x -> DL.head x == "#CHROM") dataonly)
    --Remove "INFO" from dataheader.
    let finaldataheader = DLS.splitWhen (\x -> x == "INFO") dataheader
    --Insert finalheaders into finaldataheader.
    let truefinalheader = DL.concat (insertSubfields finaldataheader finalheaders)
    --Remove CSQ from truefinalheader.
    let actualtruefinalheader = DLS.splitWhen (\x -> x == "CSQ") truefinalheader
    --Relicate headinfodataonly the correct number of times.
    let headreplicateddata = dataReplicator headinfodataonly (DL.concat (DL.map (DL.map (DL.length)) splitlastsplitcomma))
    --Replicate tailinfodataonly the correct number of times.
    let tailreplicateddata = dataReplicator tailinfodataonly (DL.concat (DL.map (DL.map (DL.length)) splitlastsplitcomma))
    --Concatenate splitlastsplitcomma.
    let concatsplitlastsplitcomma = (DL.concat (DL.concat splitlastsplitcomma))
    --Combined replicateddata and splitlastsplitcomma.
    let finalizeddata = dataCombinator headreplicateddata concatsplitlastsplitcomma tailreplicateddata
    --Add allmetadata to actualtruefinalheader and finalizeddata.
    let finalfinaldata = [mapNotLast (++ "\n") allmetadata] ++ (DL.map (mapNotLast (++ "\t")) actualtruefinalheader)
                                                            ++ (DL.map (mapNotLast (++ "\t")) (notApplicableAdder finalizeddata))
    --Print the file to stdout (cat) or to a file.
    if outputf /= "NONE"
        --Check to see if outfile is to be gzipped.
        then if gzipout
            then do
                _ <- gzipPrintFile outputf finalfinaldata
                return ()
            else noGzipPrintFile outputf finalfinaldata
    else catFile finalfinaldata

--processArgsAndFilesTvcfVcf -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndFilesTvcfVcf :: (String,String,Bool,Bool,String,FilePath) -> IO ()
processArgsAndFilesTvcfVcf ([],[],_,_,[],[]) = return ()
processArgsAndFilesTvcfVcf (inf,outf,gzipin,gzipout,outputf,inputfile) = do
    --Check to see if inputfile is gzip compressed.
    if gzipin
        then do --Decompress the file.
                SP.readProcess "gunzip" [inputfile] []
                --Read the decompressed file.
                gunzippedfile <- SIO.readFile (DLE.dropSuffix ".gz" inputfile)
                --Apply lineFeed function to inputfile.
                let gprocessedfile = lineFeed gunzippedfile
                print gprocessedfile
        else do --Read in the file.
                readinputfile <- SIO.readFile inputfile
                --Apply lineFeed function to inputfile.
                let processedfile = lineFeed readinputfile
                print processedfile

--processArgsAndContentsTvcfVcf -> This function will
--walk through each of the command-line
--arguments and files provided by the user.
processArgsAndContentsTvcfVcf :: (String,String,Bool,Bool,String,String) -> IO ()
processArgsAndContentsTvcfVcf ([],[],_,_,[],[]) = return ()
processArgsAndContentsTvcfVcf (inf,outf,gzipin,gzipout,outputf,content) = do
    --Apply lineFeed function to inputfile.
    let processedfile = lineFeed content
    --Grab only the data portion of processedfile.
    print "tvcfvcfcontents"

{-------------------------}
