{-=BasicVariantToolCommandLineParser (bvtcmp)=-}
{-=A Haskell-based command line parser.=-}
{-=Author: Matthew Mosior=-}
{-=Version: 1.0=-}

{-Module-}

module Bvtcmp where

{--------}

{-Syntax Extension-}

{-#LANGUAGE ScopedTypeVariables#-}

{------------------}

{-Imports-}

import Bvf
import Bvp
import Mam
import Vtbr
import Data.Semigroup as DS
import Options.Applicative as OA
import Data.Foldable as F
import Data.Monoid as M

{---------}

{-Custom CML Option Datatypes.-}

data BVT = 
    Bvf String String SH (Maybe FilePath) 
  | Bvp String String Bool Bool String (Maybe FilePath)
  | Mam FilePath
  | Vtbr String String Bool Bool String (Maybe FilePath)
  deriving (Eq,Show)

{------------------------------}

{-ParserInfo.-}

bvtParserInfo :: OA.ParserInfo BVT
bvtParserInfo = bvtInfo bvtParser "Basic Variant Tools: Command-line toolkit for genomic datasets.\n\
                                  \Basic Variant Tools, Copyright (c) 2019 Matthew Mosior\n\
                                  \Version 1.0.\n"
    where
        bvtParser :: OA.Parser BVT
        bvtParser = (OA.subparser . F.foldMap bvtCommand)
            [ ("bvf","Run Basic Variant Filter (bvf).",bvfP)
            , ("bvp","Run Basic Variant Parser (bvp).",bvpP)
            , ("mam","Run Move Annotate Merge (mam).",mamP)
            , ("vtbr","Run Variant to bam-readcount (vtbr).",vtbrP)
            ]
        
        bvfP  = Bvf <$> filterFieldsOpt <*> outputFileOpt <*> stripHeaderFlag' <*> inputFileArgOpt 
        bvpP  = Bvp <$> inFormatBvpOpt <*> outFormatBvpOpt <*> gzipInSwitch <*> gzipOutSwitch <*> outputFileOpt <*> inputFileArgOpt
        mamP  = Mam <$> inputFileArgNonOpt
        vtbrP = Vtbr <$> inFormatVtbrOpt <*> outFormatVtbrOpt <*> gzipInSwitch <*> gzipOutSwitch <*> outputFileOpt <*> inputFileArgOpt
        
        --bvtInfo.--
        bvtInfo :: OA.Parser a -> String -> OA.ParserInfo a
        bvtInfo p desc = OA.info
            (OA.helper <*> p)
            (OA.fullDesc <> OA.progDesc desc)
        -----------

        --bvtCommand.--
        bvtCommand (cmdName,desc,parser) =
            OA.command cmdName (bvtInfo parser desc)
        ---------------      
 
--strArguments.
inputFileArgOpt = optional $ OA.strArgument
    (M.mconcat
        [ OA.help    "Input file.",
          OA.metavar "INPUTFILEOPT" ])

inputFileArgNonOpt = OA.strArgument
    (M.mconcat
        [ OA.help    "Input file.",
          OA.metavar "INPUTFILENONOPT" ])
--------------
 
--strOptions.
outputFileOpt :: OA.Parser String
outputFileOpt = OA.strOption
    (M.mconcat
        [ OA.help    "The output file.",
          OA.short   'o',
          OA.long    "outputfile",
          OA.value   "NONE",
          OA.metavar "OUTFILE" ])
 
filterFieldsOpt :: OA.Parser String
filterFieldsOpt = OA.option parseFilterFieldsBvf
    (M.mconcat
        [ OA.help    "The fields to filter on.",
          OA.short   'F',
          OA.long    "filterfields",
          OA.value   "NONE",
          OA.metavar "FIELDS" ])
   
inFormatBvpOpt :: OA.Parser String
inFormatBvpOpt = OA.option parseInFormatBvp
    (M.mconcat
        [ OA.help    "The format of the input file.",
          OA.short   'I',
          OA.long    "informat",
          OA.metavar "IN" ])
        
outFormatBvpOpt :: OA.Parser String
outFormatBvpOpt = OA.option parseOutFormatBvp
    (M.mconcat
        [ OA.help    "The format of the output file.",
          OA.short   'O',
          OA.long    "outformat",
          OA.metavar "OUT" ])
        
inFormatVtbrOpt :: OA.Parser String
inFormatVtbrOpt = OA.option parseInFormatVtbr
    (M.mconcat
        [ OA.help    "The format of the input file.",
          OA.short   'I',
          OA.long    "informat",
          OA.metavar "IN" ])

outFormatVtbrOpt :: OA.Parser String
outFormatVtbrOpt = OA.option parseOutFormatVtbr
    (M.mconcat
        [ OA.help    "The format of the output file.",
          OA.short   'O',
          OA.long    "outformat",
          OA.metavar "OUT" ])
------------- 

--flag's.
stripHeaderFlag' :: OA.Parser SH
stripHeaderFlag' = stripHeaderSHESFlag' <|> stripHeaderSHSTFlag' <|> stripHeaderSHSHFlag'

stripHeaderSHESFlag' :: OA.Parser SH
stripHeaderSHESFlag' = OA.flag' SHES 
    (M.mconcat
        [ OA.help  "Strip the headers in the file (Exact, without head, or without tail).",
          OA.short 'E',
          OA.long  "stripheaderexact" ])

stripHeaderSHSTFlag' :: OA.Parser SH
stripHeaderSHSTFlag' = OA.flag' SHST
    (M.mconcat
        [ OA.help  "Strip the headers in the file (Exact, without head, or without tail).",
          OA.short 'H',
          OA.long  "stripheadertail" ])

stripHeaderSHSHFlag' :: OA.Parser SH
stripHeaderSHSHFlag' = OA.flag' SHST
    (M.mconcat
        [ OA.help  "Strip the headers in the file (Exact, without head, or without tail).",
          OA.short 'T',
          OA.long  "stripheaderhead" ])
--------

--switches.  
gzipInSwitch :: OA.Parser Bool
gzipInSwitch = OA.switch
    (M.mconcat
        [ OA.help  "Gzipped input file?",
          OA.short 'g',
          OA.long  "gzipin" ])
 
gzipOutSwitch :: OA.Parser Bool
gzipOutSwitch = OA.switch
    (M.mconcat
        [ OA.help "Gzip output file?",
          OA.short 'G',
          OA.long  "gzipout" ])      
-----------

{-BVT data parsers.-}

parseFilterFieldsBvf :: ReadM String
parseFilterFieldsBvf = OA.eitherReader $ \arg ->
    case Bvf.filterFieldsCheck arg of
        True  -> Right arg
        False -> Left ("Incorrect structure of filtration string (;:~~;).\n")

parseInFormatBvp :: ReadM String
parseInFormatBvp = OA.eitherReader $ \arg ->
    case Bvp.checkInFormat arg of 
        True  -> Right arg
        False -> Left ("Input format not recognized.\n\
                       \Appropriate mappings are: vep <-> tvep and vcf <-> tvcf.\n") 

parseOutFormatBvp :: ReadM String
parseOutFormatBvp = OA.eitherReader $ \arg ->
    case Bvp.checkOutFormat arg of
        True  -> Right arg
        False -> Left ("Output format not recognized.\n\
                       \Appropriate mappings are: vep <-> tvep and vcf <-> tvcf.\n")

parseInFormatVtbr :: ReadM String
parseInFormatVtbr = OA.eitherReader $ \arg ->
    case Vtbr.checkInFormat arg of
        True  -> Right arg
        False -> Left ("Input format not recognized.\n\
                       \Accepted input formats are vcf, vep, tvcf and tvep.\n")             
           
parseOutFormatVtbr :: ReadM String
parseOutFormatVtbr = OA.eitherReader $ \arg ->
    case Vtbr.checkOutFormat arg of
        True  -> Right arg
        False -> Left ("Output format not recognized.\n\
                       \The accepted output format is mgibed.\n")

{-------------------}
