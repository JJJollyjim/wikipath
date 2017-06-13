{-# LANGUAGE OverloadedStrings, BangPatterns #-}


module QuotedString(quotedString) where

import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8 as P
import Control.Applicative ((<|>))



escape :: Parser BS.ByteString
escape = do
  char '\\'
  BS.singleton <$> P.satisfy (\c -> c == '\\' || c == '"' || c == '\'')

{- https://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec
, plus performance optimisations -- still very slow! -}
quotedString :: Parser BS.ByteString
quotedString = (do
  char '\''
  !chars <- BS.concat <$> (many' (P.takeWhile1 (\(!w8) -> w8 /= '\\' && w8 /= '\'') <|> escape))
  char '\''
  return $ chars) <|> "''"
