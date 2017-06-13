{-# LANGUAGE OverloadedStrings #-}


module Main where
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8 as P
import Data.Attoparsec.ByteString.Lazy as PLazy
import QuotedString (quotedString)
import Util (fromRight)
import Data.Maybe (catMaybes, isJust)

insertLines :: IO [B.ByteString]
insertLines = filter (B.isPrefixOf "INSERT INTO `pagelinks` VALUES") <$> B.lines <$> B.readFile "/scratch/enwiki-20170501-pagelinks.sql"


row :: Parser (Maybe (Int, BS.ByteString))
row = do
  _ <- P.char '('
  from <- P.decimal
  _ <- P.char ','
  to_namespace <- P.decimal
  _ <- P.char ','
  to_title <- quotedString
  _ <- P.char ','
  from_namespace <- P.decimal
  _ <- P.char ')'
  -- We only care about intra-mainspace links
  return $ if (to_namespace == 0 && from_namespace == 0) then Just (from, to_title) else Nothing

line :: Parser [(Int, BS.ByteString)]
line = do
  P.string "INSERT INTO `pagelinks` VALUES "
  rows <- row `P.sepBy'` (P.char ',')
  P.string ";"
  return $ catMaybes $ rows

main :: IO ()
main = do
  lns <- insertLines
  --putStrLn $ show $ PLazy.parse line $ head lns
  --putStrLn $ show $ (isJust . PLazy.maybeResult) <$> map (PLazy.parse line) (Prelude.take 100 lns)
  Prelude.mapM_ (\(from_id, to_name) -> (putStr $! show from_id) >> (putStr " ") >> (BS.putStrLn $! to_name)) $! concat $! fromRight <$> PLazy.eitherResult <$> (PLazy.parse $! line) <$> lns
