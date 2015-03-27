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

import Data.Char
import Control.Monad

infixr 5 +++

--The monad of parsers
--------------------

newtype Parser a              =  P (String -> [(a,String)])

instance Monad Parser where
   return v                   =  P (\inp -> [(v,inp)])
   p >>= f                    =  P (\inp -> case parse p inp of
                                               []        -> []
                                               [(v,out)] -> parse (f v) out)

instance MonadPlus Parser where
   mzero                      =  P (\inp -> [])
   p `mplus` q                =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])

--Basic parsers
-------------

failure                       :: Parser a
failure                       =  mzero

item                          :: Parser Char
item                          =  P (\inp -> case inp of
                                               []     -> []
                                               (x:xs) -> [(x,xs)])

parse                         :: Parser a -> String -> [(a,String)]
parse (P p) inp               =  p inp

--Choice
------

(+++)                         :: Parser a -> Parser a -> Parser a
p +++ q                       =  p `mplus` q

--Derived primitives
------------------

sat                           :: (Char -> Bool) -> Parser Char
sat p                         =  do x <- item
                                    if p x then return x else failure

digit                         :: Parser Char
digit                         =  sat isDigit

lower                         :: Parser Char
lower                         =  sat isLower

upper                         :: Parser Char
upper                         =  sat isUpper

letter                        :: Parser Char
letter                        =  sat isAlpha

alphanum                      :: Parser Char
alphanum                      =  sat isAlphaNum

char                          :: Char -> Parser Char
char x                        =  sat (== x)

string                        :: String -> Parser String
string []                     =  return []
string (x:xs)                 =  do char x
                                    string xs
                                    return (x:xs)

many                          :: Parser a -> Parser [a]
many p                        =  many1 p +++ return []

many1                         :: Parser a -> Parser [a]
many1 p                       =  do v  <- p
                                    vs <- many p
                                    return (v:vs)

ident                         :: Parser String
ident                         =  do x  <- lower
                                    xs <- many alphanum
                                    return (x:xs)

nat                           :: Parser Int
nat                           =  do xs <- many1 digit
                                    return (read xs)

int                           :: Parser Int
int                           =  do char '-'
                                    n <- nat
                                    return (-n)
                                  +++ nat

space                         :: Parser ()
space                         =  do many (sat isSpace)
                                    return ()

--Ignoring spacing
----------------

token                         :: Parser a -> Parser a
token p                       =  do space
                                    v <- p
                                    space
                                    return v

identifier                    :: Parser String
identifier                    =  token ident

natural                       :: Parser Int
natural                       =  token nat

integer                       :: Parser Int
integer                       =  token int

symbol                        :: String -> Parser String
symbol xs                     =  token (string xs)





--------------------------------------------------------------------

identAlpha :: Parser String
identAlpha = do x  <- alphanum
                xs <- many alphanum
                return (x:xs)
                                         
idOrNum :: Parser String
idOrNum =  token identAlpha


alphanumOrDot :: Parser Char
alphanumOrDot = do s <- sat isAlphaNum
                   return s
                 +++ do s <- sat (== '_')               --updated 2014/12/04
                        return s
                 +++ do s <- sat (== '.')
                        return s

identOrDot :: Parser String
identOrDot = do x  <- letter
                xs <- many alphanumOrDot
                return (x:xs)
                                         
idOrDot :: Parser String
idOrDot =  token identOrDot

alphanumOrOther :: Parser Char                      --for Data or Type
alphanumOrOther = do s <- sat isAlphaNum
                     return s
                   +++ do s <- sat (== '_')
                          return s
                   +++ do s <- sat (== '.')
                          return s
                   +++ do s <- sat (== '-')
                          return s
                   +++ do s <- sat (== '/')
                          return s
                   +++ do s <- sat (== ':')
                          return s
                   +++ do s <- sat (== '*')
                          return s
                                      
identOrOther :: Parser String
identOrOther = do x  <- letter
                  xs <- many alphanumOrOther
                  return (x:xs)

idOrOther :: Parser String
idOrOther = token identOrOther

alphanumOrPlusMinus :: Parser Char
alphanumOrPlusMinus = do s <- sat isAlphaNum
                         return s
                       +++ do s <- sat (== '+')
                              return s
                       +++ do s <- sat (== '-')
                              return s
                       +++ do s <- sat (== '.')
                              return s

identOrPlusMinus :: Parser String
identOrPlusMinus = do xs <- many1 alphanumOrPlusMinus
                      return xs

value :: Parser String
value =  token identOrPlusMinus

intent :: Parser IntentSpec
intent = do symbol "{"
            s <- fields
            symbol "}"
            symbol "||"
            i <- intent
            return (s : i)
          +++do symbol "{"
                s <- fields
                symbol "}"
                return [s]

fields :: Parser Intent
fields = do a <- action
            s <- fields
            return (a : s)
          +++ do c <- category
                 s <- fields
                 return (c : s)
          +++ do d <- idata
                 s <- fields
                 return (d : s)
          +++ do t <- itype
                 s <- fields
                 return (t : s)
          +++ do c <- component
                 s <- fields
                 return (c : s)
          +++ do e <- extra
                 s <- fields
                 return (e : s)
          +++ do f <- flag
                 s <- fields
                 return (f : s)
          +++ return []
        

action :: Parser Field
action = do symbol "act"
            symbol "="
            act <- idOrDot
            return (Action act)


category :: Parser Field
category = do symbol "cat"
              symbol "="
              symbol "["
              cat <- idOrDot
              Category cats <- categorySub
              symbol "]"
              return (Category (cat : cats))

categorySub :: Parser Field
categorySub = do symbol ","
                 cat <- idOrDot
                 Category cats <- categorySub
                 return (Category (cat : cats))
               +++ return (Category [])

idata :: Parser Field
idata = do symbol "dat"
           symbol "="
           --dat <- symbol "non-null"
           dat <- idOrOther
           return (Data dat)
           

itype :: Parser Field
itype = do symbol "typ"
           symbol "="
           typ <- idOrOther
           --typ <- symbol "non-null"
           return (Type typ)
           
component :: Parser Field
component = do symbol "cmp"
               symbol "="
               pname <- idOrDot
               symbol "/"
               do cname <- idOrDot
                  return (Component pname cname)
                +++ do symbol "."
                       cname <- idOrDot
                       return (Component pname ("."  ++ cname))

extra :: Parser Field
extra = do symbol "["
           k <- idOrNum
           symbol "="
           t <- idOrNum
           do a <- symbol "[]"
              v <- value
              Extra e <- extraSub
              symbol "]"
              return (Extra ((k, (t ++ a), v) : e))
            +++ do v <- value
                   Extra e <- extraSub
                   symbol "]"
                   return (Extra ((k, t, v) : e)) 
           
extraSub :: Parser Field
extraSub = do symbol ","
              k <- idOrNum
              symbol "="
              t <- idOrNum
              do a <- arr
                 v <- value
                 Extra e <- extraSub
                 return (Extra ((k, (t ++ a), v) : e))
               +++ do v <- value
                      Extra e <- extraSub
                      return (Extra ((k, t, v) : e))
            +++ return (Extra [])
           
flag :: Parser Field
flag = do symbol "flg"
          symbol "["
          fl <- idOrNum
          Flag fls <- flagSub
          symbol "]"
          return (Flag (fl : fls))

flagSub :: Parser Field
flagSub = do symbol ","
             fl <- idOrDot
             Flag fls <- categorySub
             return (Flag (fl : fls))
           +++ return (Flag [])

arr :: Parser String
arr = do a1 <- symbol "[]"
         a2 <- arr
         return (a1 ++ a2)
       +++ return ""

eval :: String -> IntentSpec
eval xs = case parse intent xs of
               [(n, [])] -> n
               [(_, out)] -> error ("unused input" ++ out)
               [] -> error "invalid input"



type IntentSpec = [Intent]
type Intent = [Field]
data Field = Action String | Category [String] | Data String | Type String 
             | Component String String | Extra [(String, String, String)] | Flag [String] deriving (Show, Eq)


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

                      7 -> do flg <- flagElementsList 
                              return (Flag flg)
                      
                      8 -> do act <- actionArbitrary
                              return (Action act)
                              
                      9 -> do cat <- categoryArbitrary
                              return (Category cat)
                              
                      10 -> do dat <- dataArbitrary
                               return (Data dat)
                              
                      11 -> do typ <- typeArbitrary
                               return (Type typ)

                      12 -> do pkg <- componentsArbitrary
                               cls <- componentsArbitrary
                               return (Component pkg cls)

                      13 -> do ext <- extraArbitrary
                               return (Extra ext)

                      14 -> do flg <- flagArbitrary 
                               return (Flag flg)


--genField seed = unGen arbitrary (mkQCGen randInteger) 15 :: [Field]
--genField seed = unGen arbitrary (mkStdGen seed) 15 :: [Field]
genField seed = unGen arbitrary (mkQCGen (unsafePerformIO (getStdRandom (randomR (-9223372036854775808, 9223372036854775807))))) 15 :: [Field]

makeTestCaseOfIntentSpec :: Int -> IntentSpec
makeTestCaseOfIntentSpec count = take count [x | x <- map (removeDuplicateConstructor . genField) [1..count] ]

removeDuplicateConstructor :: Intent -> Intent
removeDuplicateConstructor [] = []
removeDuplicateConstructor (Action x : xs) = Action x : removeDuplicateConstructor (xs \\ [Action y | Action y <- xs])
removeDuplicateConstructor (Category x : xs) = removeDuplicateCategoryList (Category x) : removeDuplicateConstructor (xs \\ [Category y | Category y <- xs])
removeDuplicateConstructor (Data x : xs) = Data x : removeDuplicateConstructor (xs \\ [Data y | Data y <- xs])
removeDuplicateConstructor (Type x : xs) = Type x : removeDuplicateConstructor (xs \\ [Type y | Type y <- xs])
removeDuplicateConstructor (Component x1 x2 : xs) = Component x1 x2 : removeDuplicateConstructor (xs \\ [Component y1 y2 | Component y1 y2 <- xs])
removeDuplicateConstructor (Extra x : xs) = removeDuplicateExtraKeys (Extra x) : removeDuplicateConstructor (xs \\ [Extra y | Extra y <- xs])
removeDuplicateConstructor (Flag x : xs) = Flag x : removeDuplicateConstructor (xs \\ [Flag y | Flag y <- xs])

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/=x) xs)

removeDuplicateCategoryList :: Field -> Field
removeDuplicateCategoryList (Category (xs)) = Category (rmdups xs)

rmdupsExtra :: Eq a => [(a,b,c)] -> [(a,b,c)]
rmdupsExtra [] = []
rmdupsExtra ((k,t,v) : xs) = (k,t,v) : rmdupsExtra (filter ((/=k).fstExtra) xs)

fstExtra :: (a,b,c) -> a
fstExtra (x,_,_) = x 

removeDuplicateExtraKeys :: Field -> Field
removeDuplicateExtraKeys (Extra (xs)) = Extra (rmdupsExtra xs)

replaceComponent :: IntentSpec -> IntentSpec -> IntentSpec
replaceComponent _ [] = []
replaceComponent (s:ss) (d:ds) = addAndRemoveComponentFix [Component x1 x2 | Component x1 x2 <- s] d : replaceComponent (s:ss) ds

addAndRemoveComponentFix :: Intent -> Intent -> Intent
addAndRemoveComponentFix (Component x1 x2 : ss) ds = Component x1 x2 : (ds \\ [Component y1 y2 | Component y1 y2 <- ds])

makeAdbCommand :: IntentSpec -> String
makeAdbCommand [] = []
makeAdbCommand (i:is) = "adb shell am start" ++ makeIntentCommand i ++ "\n" ++ makeAdbCommand is

makeIntentCommand :: Intent -> String
makeIntentCommand [] = []
makeIntentCommand (Action x : xs) = " -a " ++ x ++ makeIntentCommand xs
makeIntentCommand (Category x : xs) = makeCagegory x ++ makeIntentCommand xs
makeIntentCommand (Data x : xs) = " -d " ++ x ++ makeIntentCommand xs
makeIntentCommand (Type x : xs) = " -t " ++ x ++ makeIntentCommand xs
makeIntentCommand (Component x1 x2 : xs) = " -n " ++ x1 ++ "/" ++ x2 ++ makeIntentCommand xs
makeIntentCommand (Extra x : xs)  = makeExtra x ++ makeIntentCommand xs
makeIntentCommand (Flag x : xs) = makeIntentCommand xs

makeCagegory :: [String] -> String
makeCagegory [] = []
makeCagegory (x:xs) = " -c " ++ x ++ makeCagegory xs

makeExtra :: [(String, String, String)] -> String
makeExtra [] = []
makeExtra ((k,t,v):xs) = case t of
                                "string" -> " --es " ++ k ++ " " ++ v ++ makeExtra xs
                                "boolean" -> " --ez " ++ k ++ " " ++ v ++ makeExtra xs
                                "integer" -> " --ei " ++ k ++ " " ++ v ++ makeExtra xs
                                "long" -> " --el " ++ k ++ " " ++ v ++ makeExtra xs
                                "float" -> " --ef " ++ k ++ " " ++ v ++ makeExtra xs
                                "URI" -> " --eu " ++ k ++ " " ++ v ++ makeExtra xs
                                "component name" -> " --ecn " ++ k ++ " " ++ v ++ makeExtra xs
                                "array of integers" -> " --eia " ++ k ++ " " ++ v ++ makeExtra xs
                                "array of longs" -> " --ela " ++ k ++ " " ++ v ++ makeExtra xs
                                "array of floats" -> " --efa " ++ k ++ " " ++ v ++ makeExtra xs
                                _ ->  " --es " ++ k ++ " " ++ v ++ makeExtra xs


addIntentSpecUsingInputIntent :: IntentSpec -> IntentSpec -> IntentSpec
addIntentSpecUsingInputIntent _ [] = []
addIntentSpecUsingInputIntent (s:ss) (d:ds) = (d ++ s) : addIntentSpecUsingInputIntent (s:ss) ds

removeDuplicateConstructorIntentSpec :: IntentSpec -> IntentSpec
removeDuplicateConstructorIntentSpec xs = map removeDuplicateConstructor xs

make :: Int -> String -> IO ()
make count spec = putStr (makeAdbCommand addIntentUsingInput)
                  where inputSpec = (eval spec)
                        replaceComp = (replaceComponent inputSpec (makeTestCaseOfIntentSpec count))
                        addIntentUsingInput = removeDuplicateConstructorIntentSpec (addIntentSpecUsingInputIntent inputSpec replaceComp)

