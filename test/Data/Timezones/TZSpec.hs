module Data.Timezones.TZSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Prelude (IO)
import Prelude (read)
import Prelude (($))

import Data.Time (UTCTime)
import Data.Timezones.TZ (tzEST, asUTC)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Testing timezones handling" $ do

    context "timezone conversions" $
        it "should return next round minute" $ 
            asUTC tzEST "2014-10-28 13:12:34"
        `shouldBe` (read "2014-10-28 18:12:34" :: UTCTime)

    context "timezone conversions" $
        it "should return next round minute" $ 
            asUTC tzEST "2014-10-28 23:12:34"
        `shouldBe` (read "2014-10-29 04:12:34" :: UTCTime)

    