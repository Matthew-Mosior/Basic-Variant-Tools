{-=BasicVariantTools (BVT): A Haskell-based command-line tool=-}
{-=providing tools for transforming, analyzing, and filtering=-}
{-=files related to sequencing pipeline output.=-}
{-=Author: Matthew Mosior=-}
{-=Version: 1.0=-}

{-Module-}

module Main where

{--------}

{-Imports-}

import Bvf
import Bvp
import Bvs
import Mam
import Bvtcmp
import Vtbr
import Data.Maybe 
import Data.Semigroup 
import Options.Applicative 
import System.FilePath

{---------}

{-Main function.-}

main :: IO ()
main = do
    cmdin <- execParser Bvtcmp.bvtParserInfo
    --Walk through flags, options, and arguments.
    case cmdin of
        --Bvf.
        Bvf ff outputf stripheader inputfile -> do
            --See if files is null
            if null inputfile
                then do --Get stdin.
                        bvfcontents <- getContents
                        --Run args and contents through processArgsandContents.
                        Bvf.processArgsAndContents (ff,outputf,stripheader,bvfcontents)
                else do --Run args and files through processArgsandFiles.
                        Bvf.processArgsAndFiles (ff,outputf,stripheader,fromJust inputfile)
        --Bvp.
        Bvp inf outf gzipin gzipout outputf inputfile -> do
            --See if files is null.
            if null inputfile
                then do --Get stdin.
                        bvpcontents <- getContents
                        --Vep or vcf parsing pipeline?
                        if inf == "vep" && outf == "tvep"
                            then do --Run args and contents through processArgsAndContentsVepTvep.
                                    Bvp.processArgsAndContentsVepTvep (inf,outf,gzipin,gzipout,outputf,bvpcontents)
                            else if inf == "tvep" && outf == "vep"
                                then do --Run args and contents through processArgsAndContentsTvepVep.
                                        Bvp.processArgsAndContentsTvepVep (inf,outf,gzipin,gzipout,outputf,bvpcontents)
                                else if inf == "vcf" && outf == "tvcf"
                                    then do --Run args and contents through processArgsAndContentsVcfTvcf.
                                            Bvp.processArgsAndContentsVcfTvcf (inf,outf,gzipin,gzipout,outputf,bvpcontents)
                                    else do --Run args and contents through processArgsAndContentsTvcfVcf.
                                            Bvp.processArgsAndContentsTvcfVcf (inf,outf,gzipin,gzipout,outputf,bvpcontents)
                        else do --Vep or vcf parsing pipeline?
                                if inf == "vep" && outf == "tvep"
                                    then do --Run args and contents through processArgsAndFilesVepTvep.
                                            Bvp.processArgsAndFilesVepTvep (inf,outf,gzipin,gzipout,outputf,fromJust inputfile)
                                    else if inf == "tvep" && outf == "vep"
                                        then do --Run args and contents through processArgsAndFilesTvepVep.
                                                Bvp.processArgsAndFilesTvepVep (inf,outf,gzipin,gzipout,outputf,fromJust inputfile)
                                        else if inf == "vcf" && outf == "tvcf"
                                            then do --Run args and contents through processArgsAndFilesVcfTvcf.
                                                    Bvp.processArgsAndFilesVcfTvcf (inf,outf,gzipin,gzipout,outputf,fromJust inputfile)
                                            else do --Run args and contents through processArgsAndFilesTvcfVcf.
                                                    Bvp.processArgsAndFilesTvcfVcf (inf,outf,gzipin,gzipout,outputf,fromJust inputfile)
        --Bvs.
        Bvs inf gzipin gzipout outputf mgivariantf inputfile -> do
            --See if file is null.
            if null inputfile
                then do --Get stdin.
                        bvscontents <- getContents
                        --Vcf or Tvcf pipeline?
                        if inf == "vcf"  
                            then do --Run args and contents through processArgsAndContentsVcfTvcf.
                                    Bvs.processArgsAndContentsVcfMgibed (inf,gzipin,gzipout,outputf,mgivariantf,bvscontents)
                            else do --Run args and contents through processArgsAndContentsTvcfVcf.
                                    Bvs.processArgsAndContentsTvcfMgibed (inf,gzipin,gzipout,outputf,mgivariantf,bvscontents)
            else do --Vcf or Tvcf pipeline?
                    if inf == "vcf"
                         then do --Run args and contents through processArgsAndFilesVcfTvcf.
                                 Bvs.processArgsAndFilesVcfMgibed (inf,gzipin,gzipout,outputf,mgivariantf,fromJust inputfile)
                         else do --Run args and contents through processArgsAndFilesTvcfVcf.
                                 Bvs.processArgsAndFilesTvcfMgibed (inf,gzipin,gzipout,outputf,mgivariantf,fromJust inputfile)
        --Mam.
        Mam inputfile -> do
            --Read inputfile.
            mamreadinputfile <- readFile inputfile
            --Chunk the file.
            let mamfilechunks = (Mam.fileChunker
                                (words mamreadinputfile))
            --Prepare to create the final directorie(s).
            let mamprefinaldirectory = Mam.prepareCreateFinalDirectory mamfilechunks
            --Run IO functions.
            Mam.createFinalDirectory mamprefinaldirectory
            Mam.variantsAnnotatedMover mamfilechunks 1
            Mam.variantsAnnotatedPipeline mamfilechunks 1
            Mam.variantsAnnotatedMerge mamfilechunks 1
        --Vtbr.
        Vtbr inf outf gzipin gzipout outputf inputfile -> do     
            --See if files is null.
            if null inputfile
                then do --Get stdin.
                        vtbrcontents <- getContents
                        --mgibed parsing pipeline?
                        if inf == "vep" && outf == "mgibed"
                            then do --Run args and contents through processArgsAndContentsVepTvep.
                                    Vtbr.processArgsAndContentsVepMgibed (inf,outf,gzipin,gzipout,outputf,vtbrcontents)
                            else if inf == "tvep" && outf == "mgibed"
                                then do --Run args and contents through processArgsAndContentsTvepVep.
                                        Vtbr.processArgsAndContentsTvepMgibed (inf,outf,gzipin,gzipout,outputf,vtbrcontents)
                                else if inf == "vcf" && outf == "mgibed"
                                    then do --Run args and contents through processArgsAndContentsVcfTvcf.
                                            Vtbr.processArgsAndContentsVcfMgibed (inf,outf,gzipin,gzipout,outputf,vtbrcontents)
                                    else do --Run args and contents through processArgsAndContentsTvcfVcf.
                                            Vtbr.processArgsAndContentsTvcfMgibed (inf,outf,gzipin,gzipout,outputf,vtbrcontents)
                        else do --mgibed parsing pipeline?
                                if inf == "vep" && outf == "mgibed"
                                    then do --Run args and contents through processArgsAndFilesVepTvep.
                                            Vtbr.processArgsAndFilesVepMgibed (inf,outf,gzipin,gzipout,outputf,fromJust inputfile)
                                    else if inf == "tvep" && outf == "mgibed"
                                        then do --Run args and contents through processArgsAndFilesTvepVep.
                                                Vtbr.processArgsAndFilesTvepMgibed (inf,outf,gzipin,gzipout,outputf,fromJust inputfile)
                                        else if inf == "vcf" && outf == "mgibed"
                                            then do --Run args and contents through processArgsAndFilesVcfTvcf.
                                                    Vtbr.processArgsAndFilesVcfMgibed (inf,outf,gzipin,gzipout,outputf,fromJust inputfile)
                                            else do --Run args and contents through processArgsAndFilesTvcfVcf.
                                                    Vtbr.processArgsAndFilesTvcfMgibed (inf,outf,gzipin,gzipout,outputf,fromJust inputfile)            
