{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for infix types
module Syntax.Tree.TypeInfix where

import Syntax.FreeVariables (FreeTypeVariables (..))
import Syntax.Parser
  ( Parser,
    asum,
    position,
    token,
    (<**>),
    (<|>),
  )
import Syntax.Position (Position)
import Syntax.Printer (Printed)
import qualified Syntax.Printer as Printer
import qualified Syntax.Tree.Marked as Marked
import Syntax.Tree.Type (Type)
import qualified Syntax.Tree.Type as Type
import qualified Syntax.Variable as Variable
import Prelude hiding (head, print, tail)

data Infix position
  = Type !(Type position)
  | -- |
    -- > t ':+ t
    Infix
      { startPosition :: !position,
        left :: !(Type position),
        operator :: !(Marked.QualifiedConstructor position),
        right :: !(Infix position)
      }
  | -- |
    -- > t : t
    InfixCons
      { startPosition :: !position,
        head :: !(Type position),
        operatorPosition :: !position,
        tail :: !(Infix position)
      }
  | -- |
    -- > t :+: t
    Operator
      { startPosition :: !position,
        left :: !(Type position),
        typeOperator :: !(Marked.QualifiedConstructorIdentifier position),
        right :: !(Infix position)
      }
  deriving (Show)

toType :: Infix Position -> Type Position
toType = \case
  Type typex -> typex
  Infix {startPosition, left, operator, right} ->
    Type.Infix
      { startPosition,
        left,
        operator,
        right
      }
  InfixCons {startPosition, head, operatorPosition, tail} ->
    Type.InfixCons
      { startPosition,
        head,
        operatorPosition,
        tail
      }
  Operator {startPosition, left, typeOperator, right} ->
    Type.Operator
      { startPosition,
        left,
        typeOperator,
        right
      }

instance FreeTypeVariables Infix where
  freeTypeVariables = \case
    Type typex -> freeTypeVariables typex
    Infix {left, right} -> freeTypeVariables left <> freeTypeVariables right
    InfixCons {head, tail} -> freeTypeVariables head <> freeTypeVariables tail
    Operator {left, right} -> freeTypeVariables left <> freeTypeVariables right

parse :: Parser (Infix Position)
parse =
  Type.parse2
    <**> asum
      [ make <$ token "'" <*> position <*> operator <*> parse,
        -- A type operator: a type named by a symbol.
        typeOperator <$> position <*> Variable.parseTypeOperator <*> parse,
        pure Type
      ]
  where
    operator = Left <$> Variable.parseOperator <|> Right <$> token ":"
    make operatorPosition (Left operator) right left =
      Infix
        { startPosition = Type.startPosition left,
          left,
          operator = operatorPosition Marked.:@ operator,
          right
        }
    make operatorPosition (Right ()) tail head =
      InfixCons
        { startPosition = Type.startPosition head,
          head,
          operatorPosition,
          tail
        }
    typeOperator operatorPosition name right left =
      Operator
        { startPosition = Type.startPosition left,
          left,
          typeOperator = operatorPosition Marked.:@ name,
          right
        }

print :: Infix unit -> Printed
print = \case
  Infix {left, operator, right} -> mconcat [Type.print2 left, Marked.printOperator operator, print right]
  InfixCons {head, tail} -> mconcat [Type.print2 head, Printer.token ":", print tail]
  Operator {left, typeOperator, right} -> mconcat [Type.print2 left, Marked.print typeOperator, print right]
  Type typex -> Type.print2 typex
