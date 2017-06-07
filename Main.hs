{-# LANGUAGE OverloadedStrings #-}


module Main where
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8 as P
import Data.Attoparsec.ByteString.Lazy as PLazy
import Control.Applicative ((<|>))
import Data.Word (Word8)
import Data.Maybe (isJust)

data Row = Row Int Int BS.ByteString deriving Show

insertLines :: IO [B.ByteString]
insertLines = filter (B.isPrefixOf "INSERT INTO `page` VALUES") <$> B.lines <$> B.readFile "/scratch/enwiki-20170501-page.sql"


row :: Parser Row
row = do
  _ <- P.char '('
  id <- P.decimal
  _ <- P.char ','
  namespace <- P.decimal
  _ <- P.char ','
  title <- quotedString
  _ <- P.char ','
  _ <- quotedString
  _ <- P.char ','
  _ <- P.decimal
  _ <- P.char ','
  _ <- P.decimal
  _ <- P.char ','
  _ <- P.decimal
  _ <- P.char ','
  _ <- P.double
  _ <- P.char ','
  _ <- P.string "NULL" <|> quotedString
  _ <- P.char ','
  _ <- P.string "NULL" <|> quotedString
  _ <- P.char ','
  _ <- P.decimal
  _ <- P.char ','
  _ <- P.decimal
  _ <- P.char ','
  _ <- quotedString
  _ <- P.char ','
  _ <- P.string "NULL" <|> quotedString
  _ <- P.char ')'
  return $ Row id namespace title

escape :: Parser BS.ByteString
escape = do
  char '\\'
  BS.singleton <$> P.satisfy (\c -> c == '\\' || c == '"' || c == '\'')

{- https://stackoverflow.com/questions/24106314/parser-for-quoted-string-using-parsec -}
quotedString :: Parser BS.ByteString
quotedString = do
  char '\''
  chars <- BS.concat <$> many' (P.takeWhile1 (\w8 -> w8 /= '\\' && w8 /= '\'') <|> escape)
  char '\''
  return $ chars

isRight :: Either a b -> b
isRight (Right x) = x
isRight _ = undefined

line :: Parser [Row]
line = do
  P.string "INSERT INTO `page` VALUES "
  xs <- row `P.sepBy'` (P.char ',')
  P.string ";"
  return $ xs


main :: IO ()
main = do
  lns <- insertLines
  putStrLn $ show $ (isJust . PLazy.maybeResult) <$> map (PLazy.parse line) (Prelude.take 100 lns)
