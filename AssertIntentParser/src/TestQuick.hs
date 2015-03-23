module TestQuick where

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import System.Random
import Data.Char
import Data.List
import Control.Monad
import System.IO.Unsafe
import Data.Time.LocalTime

import ActionElements
import CategoryElements
import DataElements
import TypeElements 
import ComponentsElements
import ExtraElements
import FlagElements

type IntentSpec = [Intent]
type Intent = [Field]
data Field = Action String | Category [String] | Data String | Type String 
             | Component String String | Extra [(String, String)] | Flag deriving (Show, Eq)


instance Arbitrary Field where
  arbitrary = do n <- choose (1,13) :: Gen Int
                 case n of
                      1 -> do act <- actionElements
                              return (Action act)
                              
                      2 -> do cat <- categoryElements
                              return (Category [cat])
                              
                      3 -> do dat <- dataElements
                              return (Data dat)
                              
                      4 -> do typ <- typeElements
                              return (Type typ)
                              
                      5 -> do pkg <- packageElements
                              cls <- classElements
                              return (Component pkg cls)
                              
                      6 -> do key <- extraKeyElements
                              typ <- extraTypeElements
                              return (Extra [(key, typ)])
                              
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
                      
genField seed = unGen arbitrary (mkStdGen seed) 15 :: [Field]

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











