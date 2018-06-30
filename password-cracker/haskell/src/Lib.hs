module Lib
    ( decompose
    ) where

import           Control.Applicative
import           Data.Char

newtype Parser a = P (String -> Maybe (a,String))

parse :: Parser a -> String -> Maybe (a,String)
parse (P p) inp = p inp

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap g p = P (\inp -> case parse p inp of
                            Nothing      -> Nothing
                            Just (v,out) -> Just (g v, out))

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure v = P (\inp -> Just (v,inp))

    -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    pg <*> px = P (\inp -> case parse pg inp of
                                Nothing      -> Nothing
                                Just (g,out) -> parse (fmap g px) out)

instance Monad Parser where
    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    p >>= f = P (\inp -> case parse p inp of
                            Nothing      -> Nothing
                            Just (v,out) -> parse (f v) out)

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\_ -> Nothing)

    -- (<|>) :: Parser a -> Parser a -> Parser a
    p <|> q = P (\inp -> parse p inp <|> parse q inp)

item :: Parser Char
item = P (\inp -> case inp of
                    []     -> Nothing
                    (x:xs) -> Just (x,xs))

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

password :: [String] -> Parser String
password ws = foldl1 (<|>) (fmap string ws)

passwords :: [String] -> Parser [String]
passwords ws = some (password ws)

decompose :: String -> [String] -> Maybe [String]
decompose attempt pwds = case parse (passwords pwds) attempt of
                            Just (ws,[]) -> Just ws
                            _            -> Nothing
