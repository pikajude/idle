{-# LANGUAGE TupleSections #-}

module Idle.Ore.Parse where

import Control.Applicative
import qualified Data.Text as T
import Graphics.Vty.Attributes
import Text.Trifecta

keyToImage :: [(Char, Integer)] -> String -> String -> [(T.Text, Attr)]
keyToImage key = zipWith colorize where
    colorize c ' ' = (T.singleton c, def_attr)
    colorize c g | Just num <- lookup g key
        = (T.singleton c, Attr KeepCurrent (SetTo (ISOColor $ fromIntegral num)) KeepCurrent)
    colorize _c s = error $ "Unmatched key: " ++ [s]

oreParser :: Parser [(T.Text, Attr)]
oreParser = do
    cs <- manyTill colorBinding separator
    im <- image
    k <- keyFor im cs
    return $ keyToImage cs im k
    where
        image = manyTill anyChar separator
        keyFor i cs = concat <$> count (length $ lines i) (keyLine cs)
        keyLine cs = do
            ms <- many (char ' ' <|> oneOf (map fst cs))
            newline
            return $ ms ++ " "
        separator = string "---\n"
        colorBinding = do
            k <- alphaNum
            string ": "
            num <- decimal
            newline
            return (k,num)
