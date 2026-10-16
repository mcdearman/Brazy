module ForeignExport where

-- A function of Haskell is not yet given to C.
foreign export ccall "twice" twice :: Int -> Int

twice :: Int -> Int
twice x = x * 2
