module IntentParser where

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
                 +++ do s <- sat (== '.')
                        return s

identOrDot :: Parser String
identOrDot = do x  <- letter
                xs <- many alphanumOrDot
                return (x:xs)
                                         
idOrDot :: Parser String
idOrDot =  token identOrDot

{-
alphanumOrDotOrSpace :: Parser Char
alphanumOrDotOrSpace = do s <- sat isAlphaNum
                          return s
                        +++ do s <- sat (== '.')
                               return s
                        +++ do s <- sat (== ' ')
                               return s

identOrDotOrSpace :: Parser String
identOrDotOrSpace = do xs <- many alphanumOrDotOrSpace
                       return xs

idOrDotOrSpace :: Parser String
idOrDotOrSpace =  token identOrDotOrSpace
-}

intent :: Parser String
intent = do symbol "{"
            s <- fields
            symbol "}"
            symbol "||"
            i <- intent
            return ("{" ++ s ++ "} || " ++ i)
          +++do symbol "{"
                s <- fields
                symbol "}"
                return ("{" ++ s ++ "}")

fields :: Parser String
fields = do a <- action
            s <- fields
            return (a ++ s)
          +++ do c <- category
                 s <- fields
                 return (c ++ s)
          +++ do d <- idata
                 s <- fields
                 return (d ++ s)
          +++ do t <- itype
                 s <- fields
                 return (t ++ s)
          +++ do c <- component
                 s <- fields
                 return (c ++ s)
          +++ do e <- extra
                 s <- fields
                 return (e ++ s)
          +++ do f <- flag
                 s <- fields
                 return (f ++ s)
          +++ return ""
        

action :: Parser String
action = do symbol "act"
            symbol "="
            act <- idOrDot
            return ("act=" ++ act ++ " ")


category :: Parser String
category = do symbol "cat"
              symbol "="
              symbol "["
              cat <- idOrDot
              cats <- categorySub
              symbol "]"
              return ("cat=[" ++ cat ++ cats ++ "] ")

categorySub :: Parser String
categorySub = do symbol ","
                 cat <- idOrDot
                 cats <- categorySub
                 return ("," ++ cat ++ cats)
               +++ return ""

idata :: Parser String
idata = do symbol "dat"
           symbol "="
           dat <- symbol "non-null"
           return ("dat=" ++ dat ++ " ")
           

itype :: Parser String
itype = do symbol "typ"
           symbol "="
           typ <- symbol "non-null"
           return ("typ=" ++ typ ++ " ")
           
component :: Parser String
component = do symbol "cmp"
               symbol "="
               pname <- idOrDot
               symbol "/"
               do cname <- idOrDot
                  return ("cmp=" ++ pname ++ "/" ++ cname ++ " ")
                +++ do symbol "."
                       cname <- idOrDot
                       return ("cmp=" ++ pname ++ "/." ++ cname ++ " ")

extra :: Parser String
extra = do symbol "["
           i <- idOrNum
           e <- extraSub
           symbol "]"
           return (" [" ++ i ++ e ++ "] ")
           
extraSub :: Parser String
extraSub = do symbol ","
              i <- idOrNum
              is <- extraSub
              return (", " ++ i ++ is)
            +++ return ""
               
           
flag :: Parser String
flag = do symbol "flg"
          return "flg"

eval :: String -> String
eval xs = case parse intent xs of
               [(n, [])] -> n
               [(_, out)] -> error ("unused input" ++ out)
               [] -> error "invalid input"

















