import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Numeric.Natural (Natural)
import PRF
import Test.Syd
import Test.Syd.Hedgehog

main :: IO ()
main = sydTest $ do
  describe "primitive-recursive-functions" $ do
    describe "Axioms" $ do
      describe "C^k_n" $ do
        specify "C^1_0 returns 0 for every input" $
          property $ do
            x <- forAll $ peano (Range.linear 0 100)
            runFunc (c 1 0) [x] === Z

        specify "C^1_1 returns 1 for every input" $
          property $ do
            x <- forAll $ peano (Range.linear 0 100)
            runFunc (c 1 1) [x] === S Z

        specify "C^0_3 is a constant 3" $
          runFunc (c 0 3) [] `shouldBe` (S . S . S) Z

        specify "C^k_n always returns n" $
          property $ do
            k <- forAll $ Gen.integral (Range.linear 0 100)
            n <- forAll $ Gen.integral (Range.linear 0 100)
            let k' = fromIntegral k
            xs <- forAll $ Gen.list (Range.constant k' k') $ peano (Range.linear 0 100)
            runFunc (c k n) xs === fromNat n

      describe "P^k_n" $ do
        specify "P^1_1 is the identity function" $ do
          property $ do
            x <- forAll $ peano (Range.linear 0 100)
            runFunc (p 1 1) [x] === x

        specify "P^2_1 is the left projection on a pair" $ do
          property $ do
            x <- forAll $ peano (Range.linear 0 100)
            y <- forAll $ peano (Range.linear 0 100)
            runFunc (p 2 1) [x, y] === x

        specify "P^2_2 is the right projection on a pair" $ do
          property $ do
            x <- forAll $ peano (Range.linear 0 100)
            y <- forAll $ peano (Range.linear 0 100)
            runFunc (p 2 2) [x, y] === y

      describe "g • h" $ do
        specify "S • S adds 2 to its input" $ do
          property $ do
            x <- forAll $ Gen.integral (Range.linear 0 100)
            runFunc (s • [s]) [fromNat x] === fromNat (x + 2)

        specify "S • C^1_0 returns 1 for every input" $ do
          property $ do
            x <- forAll $ peano (Range.linear 0 100)
            runFunc (s • [c 1 0]) [x] === S Z

      describe "rho(g, h)" $ do
        pure ()

{----- Helpers -----}

peano :: Range Natural -> Gen PN
peano = fmap fromNat . Gen.integral
