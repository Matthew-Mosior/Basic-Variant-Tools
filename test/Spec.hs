{-=Basic-Variant-Tools (BVT): Test Suite=-}
{-=Author: Matthew Mosior=-}
{-=Version: 1.0=-}
{-=Synopsis:  This Haskell Script will provide=-}
{-=property-based and traditional unit-testing.=-}

{-Imports-}

import Bvf
import Bvp
import Mam
import Vtbr
import Data.List as DL
import Data.Char as DC
import Data.List.Split as DLS
import Data.Ord as DO
import System.Random as SR
import System.IO.Unsafe as SIOU
import Test.Tasty as TT
import Test.Tasty.QuickCheck as TTQC
import Test.Tasty.HUnit as TTHU

{---------}

{-Test-related functions.-}

tests :: TT.TestTree
tests = TT.testGroup "Basic-Variant-Tools: Full Test Suite" [properties, unitTests]

properties :: TT.TestTree
properties = TT.testGroup "Basic-Variant-Tools: Property-Based Testing" [qcProps]

qcProps = TT.testGroup "Checked by the QuickCheck library:"
    [ TTQC.testProperty "Bvf -> isAlphaList check." $
          Bvf.isAlphaList (DL.take 10 $ SR.randomRs ('a','z') $ SIOU.unsafePerformIO SR.newStdGen) == True
    , TTQC.testProperty "Bvf -> isNotAlphaList check." $
          Bvf.isNotAlphaList (DL.take 10 $ SR.randomRs ('a','z') $ SIOU.unsafePerformIO SR.newStdGen) == False
    ]

unitTests = TT.testGroup "Basic-Variant-Tools: Unit-Testing.\nChecked by the HUnit library."
    [ TTHU.testCase "List comparison" $
      [1,2,3] `compare` [1,2] @?= GT  
    ]

{-------------------------}

{-Main function.-}

main :: IO ()
main = defaultMain tests

{----------------}
