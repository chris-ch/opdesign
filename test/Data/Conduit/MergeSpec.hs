module Data.Conduit.MergeSpec where

    import SpecHelper
    
    import Prelude (Int, Integer)
    import Prelude (zipWith, drop, maybe, return)
    import Prelude (($), (<=))
    import Conduit ()
    import Conduit (yieldMany, runConduitPure, takeC)
    import Conduit (sinkList)
    import Conduit ((.|))

    import Data.Conduit.Merge (mergeC2) --, zipC2)
    
    spec :: Spec
    spec = describe "Testing Conduit merging operators" $ do
    
        context "yielding array of 10 first integers" $
            it "should yield many" $ 
                runConduitPure (yieldMany [1..10 :: Integer] .| sinkList)
            `shouldBe` [1..10]
    
        context "merging sources (assumes both are monotonic)" $
            let
                naturals1 = yieldMany [10, 20, 30, 40, 50, 60, 70, -10, -20, -10 :: Int]
                naturals2 = yieldMany [1, 2, 3, 4, 5 :: Int]
            in
            it "should zip 2 sources" $
                runConduitPure ( mergeC2 (<=) naturals1 naturals2 .| takeC 100 .| sinkList )
                `shouldBe` [1, 2, 3, 4, 5, 10, 20, 30, 40, 50, 60, 70, -10, -20, -10]
    
        -- context "zipping sources as pairs" $
        --     let
        --         naturals1 = yieldMany [2, 4, 6, 8, 16 :: Int]
        --         naturals2 = yieldMany [1, 5, 6 :: Int]
        --     in
        --     it "should zip 2 sources" $
        --         runConduitPure ( zipC2 (<=) naturals1 naturals2 .| takeC 100 .| sinkList )
        --         `shouldBe` [(Nothing, Just 1), (Just 2, Just 1), (Just 4, Just 1), (Just 4, Just 5), (Just 6, Just 5), (Just 6, Just 6), (Just 8, Just 6), (Just 16, Just 6)]
    