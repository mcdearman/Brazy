module BadNegateAfterRight where

infixr 6 <+>

(<+>) :: Int -> Int -> Int
a <+> b = a + b

-- Nor one of its precedence that groups to the right.
negative = 1 <+> - 2
