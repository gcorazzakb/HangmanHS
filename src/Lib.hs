
module Lib
    ( validate
    ) where

import Data.Char
import Data.Bifunctor (first)

validate :: String -> Bool
validate a
    | length (take 20 a) == 20 = True
    | otherwise = length (take 8 a) == 8 && length (filter id (map (\f -> f a) [hasUpperCase, hasLowerCase, hasNumber]) ) >= 2
    where
    hasUpperCase = any isUpper
    hasLowerCase = any isLower
    hasNumber = any isNumber
--
--newtype Parser a = Parser (String -> [(a, String)])
--
--parse :: Parser a -> String -> [(a, String)]
--parse (Parser f) = f
--
--instance Functor Parser where
--  fmap f (Parser a) = Parser ( map (first f) . a)
--
--instance Applicative Parser where
--  pure a = Parser (const [(a, "")])
--  (<*>) (Parser fab) (Parser a) = Parser ( id . a)
--  
--instance Monad Parser where
--  (>>=) (Parser a) amb = Parser (map (first amb) . a)
--   
--testFunc :: String
--testFunc = show B
--
--data MyAOrB = A | B
--
--instance Show MyAOrB where
--  show a = case a of A -> ""
--                     B -> " "
---- instance Monad Parser where 

fac :: Int -> Int
fac 0 = 1
fac i = i * fac (i - 1)

