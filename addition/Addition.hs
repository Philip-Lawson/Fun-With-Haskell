module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True
    it "2 + 3 is equal to 5" $ do
      (2 + 3) == 5 `shouldBe` True
    it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 7 is 3 remainder 1" $ do 
      dividedBy 22 7 `shouldBe` (3, 1)
    it "5 multiplied by 6 should be 30" $ do
      multSummation 5 6 `shouldBe` 30
    it "-5 multiplied by 6 should be -30" $ do
      multSummation (-5) 6 `shouldBe` (-30)
    it "-5 multiplied by -6 should be 30" $ do
      multSummation (-5) (-6) `shouldBe` 30
    it "n by 0 should be 0" $ do
      multSummation 5 0 `shouldBe` 0
    it "0 by n should be 0" $ do
      multSummation 0 5 `shouldBe` 0
    it "multiplication by summation should always be the same as standard multiplication" $ do
       quickCheck prop_multTest

sayHello :: IO ()
sayHello = putStrLn "Testing"

dividedBy :: Integral a => a  -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
         | n < d = (count, n)
         | otherwise = go (n - d) d (count + 1)

multSummation :: (Eq a, Num a) => a -> a -> a
multSummation 0 _ = 0
multSummation _ 0 = 0
multSummation x y = (x * signum y) + multSummation x (y - signum y)

prop_multTest :: Int -> Int -> Bool
prop_multTest x y = multSummation x y == x * y

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  frequency  [(3, return (Left a)), (1, return (Right b))]
