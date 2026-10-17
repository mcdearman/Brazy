module ForeignExport where

-- What C calls a function of Haskell is one name.
foreign export ccall "twice over" twice :: Int -> Int

twice :: Int -> Int
twice x = x * 2
