module BadNegateAfterLeft where

-- Negation cannot follow an operator of its own precedence.
negative = 1 + - 2
