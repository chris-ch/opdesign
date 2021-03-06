module OpDesign.TicksReaderSpec where

import Prelude (map)
import Prelude (($), (.))

import SpecHelper

import Codec.Archive.Zip (getEntryName)

import Data.Text (unpack)

import OpDesign.TicksReader (extractEntries, selectEntries, isTickFile)

spec :: Spec
spec = describe "Reading ticks from archive" $ do
    context "price archive reader" $ do
        it "should list archive content" $ do
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
  
            map (unpack . getEntryName) entries `shouldBe` archiveContent
  
        it "should select csv files" $ do
            entries <- extractEntries "test/data/data-small.zip"
            let csvEntries = selectEntries (isTickFile ".*/[^.]+.csv") entries
            let archiveContent =    [ "data-small/20141026.csv"
                                    , "data-small/20141027.csv"
                                    , "data-small/20141028.csv" ]
  
            map (unpack . getEntryName) csvEntries `shouldBe` archiveContent
  
