module IntentParserWithQuickCheckForSemanticData where


import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import System.Random
import Data.Char
import Data.List
import Control.Monad


type IntentSpec = [Intent]
type Intent = [Field]
data Field = Action String | Category [String] | Data String | Type String 
             | Component String String | Extra [(String, String)] | Flag deriving (Show, Eq)

type ActionF = String

instance Arbitrary ActionF where
  arbitrary = oneof ["", "", ""]

instance Arbitrary Field where
  arbitrary = oneof [ liftM Action arbitrary,
                      liftM Category arbitrary,
                      liftM Data arbitrary,
                      liftM Type arbitrary,
                      liftM2 Component arbitrary arbitrary,
                      liftM Extra arbitrary,
                      return Flag ]

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




