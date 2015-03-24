module TestQuick where

import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Data.List

import System.Random
import System.IO.Unsafe

import ActionElements
import CategoryElements
import DataElements
import TypeElements 
import ComponentsElements
import ExtraElements
import FlagElements

--cabal install quickCheck  --> 2.8
--cabal install checkers    --> 0.4.2

type IntentSpec = [Intent]
type Intent = [Field]
data Field = Action String | Category [String] | Data String | Type String 
             | Component String String | Extra [(String, String)] | Flag deriving (Show, Eq)


instance Arbitrary Field where
  arbitrary = do n <- choose (1,14) :: Gen Int
                 case n of
                      1 -> do act <- actionElements
                              return (Action act)
                              
                      2 -> do cat <- categoryElementsList
                              return (Category cat)
                              
                      3 -> do dat <- dataElements
                              return (Data dat)
                              
                      4 -> do typ <- typeElements
                              return (Type typ)
                              
                      5 -> do pkg <- packageElements
                              cls <- classElements
                              return (Component pkg cls)
                              
                      6 -> do ext <- extraTupleList
                              return (Extra ext)

                      7 -> return Flag
                      
                      8 -> do act <- arbitrary
                              return (Action act)
                              
                      9 -> do cat <- arbitrary
                              return (Category cat)
                              
                      10 -> do dat <- arbitrary
                               return (Data dat)
                              
                      11 -> do typ <- arbitrary
                               return (Type typ)
                              
                      12 -> do pkg <- arbitrary
                               cls <- arbitrary
                               return (Component pkg cls)
                              
                      13 -> do ext <- arbitrary
                               return (Extra ext)
                               
                      14 -> return Flag



--genField seed = unGen arbitrary (mkQCGen randInteger) 15 :: [Field]
--genField seed = unGen arbitrary (mkStdGen seed) 15 :: [Field]
genField seed = unGen arbitrary (mkQCGen (unsafePerformIO (getStdRandom (randomR (-9223372036854775808, 9223372036854775807))))) 15 :: [Field]

makeIntentSpecTestCase :: Int -> IntentSpec
makeIntentSpecTestCase count = take count [x | x <- map (removeSameConstructor . genField) [1..count] ]

removeSameConstructor :: Intent -> Intent
removeSameConstructor [] = []
removeSameConstructor [x] = [x]
removeSameConstructor (Action x : xs) = Action x : removeSameConstructor (xs \\ [Action y | Action y <- xs])
removeSameConstructor (Category x : xs) = Category x : removeSameConstructor (xs \\ [Category y | Category y <- xs])
removeSameConstructor (Data x : xs) = Data x : removeSameConstructor (xs \\ [Data y | Data y <- xs])
removeSameConstructor (Type x : xs) = Type x : removeSameConstructor (xs \\ [Type y | Type y <- xs])
removeSameConstructor (Component x1 x2 : xs) = Component x1 x2 : removeSameConstructor (xs \\ [Component y1 y2 | Component y1 y2 <- xs])
removeSameConstructor (Extra x : xs) = Extra x : removeSameConstructor (xs \\ [Extra y | Extra y <- xs])
removeSameConstructor (Flag : xs) = Flag : removeSameConstructor (xs \\ [Flag | Flag <- xs])




