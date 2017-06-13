{-# LANGUAGE OverloadedStrings, BangPatterns #-}


module Main where
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8 as P
import Data.Attoparsec.ByteString.Lazy as PLazy
import QuotedString (quotedString)
import Util (fromRight)
import Data.Maybe (isJust, catMaybes)
import Data.HashTable.IO
import Control.DeepSeq


insertLines :: IO [B.ByteString]
insertLines = filter (B.isPrefixOf "INSERT INTO `page` VALUES") <$> B.lines <$> B.readFile "/scratch/enwiki-20170501-page.sql"


row :: Parser (Maybe (BS.ByteString, Int))
row = do
  -- As an optimisation, the varbinaries used to store very simple non-user-entered info are skipped, instead of using the expensive quotedString parser
  _ <- P.char '('
  id' <- P.decimal
  _ <- P.char ','
  namespace <- P.decimal
  _ <- P.char ','
  title <- quotedString
  -- Major performance optimisation: we don't care about the rest of the row, and it (I hope...) doesn't contain any close brackets!!
  -- Hence we don't have to parse it in depth
  _ <- P.skipWhile (\c -> c /= ')')
  _ <- P.char ')'
  return $! (case namespace of
    0 -> Just (title, id')
    _ -> Nothing)

line :: Parser [(BS.ByteString, Int)]
line = do
  P.string "INSERT INTO `page` VALUES "
  xs <- row `P.sepBy'` (P.char ',')
  P.string ";"
  return $! catMaybes $! xs

main :: IO ()
main = do
  lns <- insertLines
  Prelude.mapM_ (\(name, id) -> (putStr $! show id) >> (putStr " ") >> (BS.putStrLn $! name)) $! concat $! fromRight <$> PLazy.eitherResult <$> (PLazy.parse $! line) <$> lns
