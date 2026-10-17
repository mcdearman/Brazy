module ForeignExportType where

-- C has nothing that is a list.
foreign export ccall size :: String -> Int

size :: String -> Int
size = length
