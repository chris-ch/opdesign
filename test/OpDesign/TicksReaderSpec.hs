module OpDesign.TicksReaderSpec where

import SpecHelper

import Codec.Archive.Zip (getEntryName)

import Data.Text (pack, unpack)

import OpDesign.TicksReader (extractEntries)

spec :: Spec
spec = describe "Reading ticks from archive" $ do
    context "price archive reader" $ do
        it "should read file content" $ do
            entries <- extractEntries "test/data/data-small.zip"
            let archiveContent =    [ "__MACOSX/._data-small"
                                    , "__MACOSX/data-small/._.DS_Store"
                                    , "__MACOSX/data-small/._20141026.csv"
                                    , "__MACOSX/data-small/._20141027.csv"
                                    , "__MACOSX/data-small/._20141028.csv"
                                    , "data-small/.DS_Store"
                                    , "data-small/20141026.csv"
                                    , "data-small/20141027.csv"
                                    , "data-small/20141028.csv" ]
  
            map (unpack . getEntryName) entries `shouldBe`  archiveContent
  
