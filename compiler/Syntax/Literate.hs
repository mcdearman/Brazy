-- |
-- Literate source, as the Haskell 2010 report's chapter 10 has it.
module Syntax.Literate (unlit) where

import Data.Text (Text)
import qualified Data.Text as Text

-- |
-- The program in a literate source: the lines that begin with a bird track,
-- @>@, and the lines between @\\begin{code}@ and @\\end{code}@. Every other
-- line is left empty, and a track is left a space, so that each line of the
-- program and its columns are where they were, for what is said of them.
unlit :: Text -> Text
unlit = Text.unlines . go False . Text.lines
  where
    go :: Bool -> [Text] -> [Text]
    go _ [] = []
    go inside (line : rest)
      | marker == Text.pack "\\begin{code}" = Text.empty : go True rest
      | marker == Text.pack "\\end{code}" = Text.empty : go False rest
      | inside = line : go True rest
      | Just code <- Text.stripPrefix (Text.pack ">") line = Text.cons ' ' code : go False rest
      | otherwise = Text.empty : go False rest
      where
        marker = Text.strip line
