A literate program, as chapter 10 of the Haskell 2010 report has it. This
module is written with bird tracks; the one it imports is written in the
LaTeX style.

> module Main where
>
> import Shapes (area, Shape (..))

What is between the code is commentary, and is not read.

> main :: IO ()
> main = do
>   print (map area [Square 2, Circle 1])
>   putStrLn "literate"
