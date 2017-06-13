module Util (fromRight) where

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = undefined
