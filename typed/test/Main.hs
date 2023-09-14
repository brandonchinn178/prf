import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Numeric.Natural (Natural)
import PRF
import Test.Syd
import Test.Syd.Hedgehog
import Prelude hiding (and, not, or, pred)

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

    describe "Examples" $ do
      describe "Add" $ do
        specify "Add(1, 7) = 8" $
          runFunc add [fromNat 1, fromNat 7] `shouldBe` fromNat 8

        specify "Add(x, y) = x + y" $
          property $ do
            x <- forAll $ Gen.integral (Range.linear 0 100)
            y <- forAll $ Gen.integral (Range.linear 0 100)
            runFunc add [fromNat x, fromNat y] === fromNat (x + y)

      describe "Double" $ do
        specify "Double(x) = x + x" $
          property $ do
            x <- forAll $ Gen.integral (Range.linear 0 100)
            runFunc double [fromNat x] === fromNat (x + x)

      describe "Mul" $ do
        specify "Mul(x, y) = x * y" $
          property $ do
            x <- forAll $ Gen.integral (Range.linear 0 100)
            y <- forAll $ Gen.integral (Range.linear 0 100)
            runFunc mul [fromNat x, fromNat y] === fromNat (x * y)

      describe "Pred" $ do
        specify "Pred(0) = 0" $
          runFunc pred [Z] `shouldBe` Z

        specify "Pred(S(n)) = n" $
          property $ do
            n <- forAll $ peano (Range.linear 0 100)
            runFunc pred [S n] === n

      describe "Sub" $ do
        specify "Sub(8, 1) = 7" $
          runFunc sub [fromNat 8, fromNat 1] `shouldBe` fromNat 7

        specify "Sub(y, 0) = y" $
          property $ do
            y <- forAll $ peano (Range.linear 0 100)
            runFunc sub [y, Z] === y

        specify "Sub(y, S(x)) = Pred(y - x)" $
          property $ do
            y <- forAll $ Gen.integral (Range.linear 1 100)
            x <- forAll $ Gen.integral (Range.linear 0 y)
            runFunc sub [fromNat y, S (fromNat x)] === runFunc pred [fromNat $ y - x]

      describe "Predicates" $ do
        describe "IsZero" $ do
          specify "IsZero(0) = true" $
            runFunc isZero [Z] `shouldBe` true

          specify "IsZero(8) = false" $
            runFunc isZero [fromNat 8] `shouldBe` false

        describe "Leq" $ do
          specify "Leq(8, 3) = false" $
            runFunc leq [fromNat 8, fromNat 3] `shouldBe` false

          specify "Leq(2, 2) = true" $
            runFunc leq [fromNat 2, fromNat 2] `shouldBe` true

          specify "Leq(1, 5) = true" $
            runFunc leq [fromNat 1, fromNat 5] `shouldBe` true

        describe "Geq" $ do
          specify "Geq(8, 3) = true" $
            runFunc geq [fromNat 8, fromNat 3] `shouldBe` true

          specify "Geq(2, 2) = true" $
            runFunc geq [fromNat 2, fromNat 2] `shouldBe` true

          specify "Geq(1, 5) = false" $
            runFunc geq [fromNat 1, fromNat 5] `shouldBe` false

        describe "If" $ do
          specify "If(true, y, z) = y" $ do
            property $ do
              y <- forAll $ peano (Range.linear 0 100)
              z <- forAll $ peano (Range.linear 0 100)
              runFunc if_ [true, y, z] === y

          specify "If(false, y, z) = z" $ do
            property $ do
              y <- forAll $ peano (Range.linear 0 100)
              z <- forAll $ peano (Range.linear 0 100)
              runFunc if_ [false, y, z] === z

        describe "And" $ do
          specify "And(true, true) = true" $
            runFunc and [true, true] `shouldBe` true

          specify "And(true, false) = false" $
            runFunc and [true, false] `shouldBe` false

          specify "And(false, true) = false" $
            runFunc and [false, true] `shouldBe` false

          specify "And(false, false) = false" $
            runFunc and [false, false] `shouldBe` false

        describe "Or" $ do
          specify "Or(true, true) = true" $
            runFunc or [true, true] `shouldBe` true

          specify "Or(true, false) = true" $
            runFunc or [true, false] `shouldBe` true

          specify "Or(false, true) = true" $
            runFunc or [false, true] `shouldBe` true

          specify "Or(false, false) = false" $
            runFunc or [false, false] `shouldBe` false

        describe "Not" $ do
          specify "Not(true) = false" $
            runFunc not [true] `shouldBe` false

          specify "Not(false) = true" $
            runFunc not [false] `shouldBe` true

        describe "Eq" $ do
          specify "Eq(8, 3) = false" $
            runFunc eq [fromNat 8, fromNat 3] `shouldBe` false

          specify "Eq(2, 2) = true" $
            runFunc eq [fromNat 2, fromNat 2] `shouldBe` true

          specify "Eq(1, 5) = false" $
            runFunc eq [fromNat 1, fromNat 5] `shouldBe` false

        describe "Lt" $ do
          specify "Lt(8, 3) = false" $
            runFunc lt [fromNat 8, fromNat 3] `shouldBe` false

          specify "Lt(2, 2) = false" $
            runFunc lt [fromNat 2, fromNat 2] `shouldBe` false

          specify "Lt(1, 5) = true" $
            runFunc lt [fromNat 1, fromNat 5] `shouldBe` true

        describe "Gt" $ do
          specify "Gt(8, 3) = true" $
            runFunc gt [fromNat 8, fromNat 3] `shouldBe` true

          specify "Gt(2, 2) = false" $
            runFunc gt [fromNat 2, fromNat 2] `shouldBe` false

          specify "Gt(1, 5) = false" $
            runFunc gt [fromNat 1, fromNat 5] `shouldBe` false

      describe "Other examples" $ do
        describe "Pow" $ do
          specify "Pow(2, 3) = 8" $
            runFunc pow [fromNat 2, fromNat 3] `shouldBe` fromNat 8

{----- Helpers -----}

peano :: Range Natural -> Gen PN
peano = fmap fromNat . Gen.integral
