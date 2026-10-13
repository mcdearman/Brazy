{-# LANGUAGE_HAZY UnorderedRecords #-}

-- |
-- Parser syntax tree for infix expressions
module Syntax.Tree.ExpressionInfix where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Syntax.Parser (Parser, asum, optional, position, some, token, (<**>), (<|>))
import Syntax.Position (Position)
import Syntax.Tree.Expression (Expression)
import qualified Syntax.Tree.Expression as Expression
import qualified Syntax.Tree.Marked as Marked
import qualified Syntax.Variable as Variable
import Prelude hiding (head, tail)

data Infix position
  = Expression !(Expression position)
  | -- |
    -- > e + e
    Infix
      { left :: !(Expression position),
        operator :: !(Marked.QualifiedName position),
        right :: !(Infix position)
      }
  | -- |
    -- > e : e
    InfixCons
      { head :: !(Expression position),
        operatorPosition :: !position,
        tail :: !(Infix position)
      }
  | -- |
    -- > - e
    Negate
      { startPosition :: !position,
        negative :: !(Infix position)
      }
  deriving (Show)

toExpression :: Infix position -> Expression position
toExpression = \case
  Expression e -> e
  Infix {left, operator, right} ->
    Expression.Infix
      { left,
        operator,
        right
      }
  InfixCons {head, operatorPosition, tail} ->
    Expression.InfixCons
      { head,
        operatorPosition,
        tail
      }
  Negate {startPosition, negative} -> Expression.Negate {startPosition, negative}

parse :: Parser (Infix Position)
parse =
  Expression.parse2
    <**> asum
      [ make <$> position <*> operator <*> parse,
        pure Expression
      ]
    <|> negate
    <$> position
    <*> (token "-" *> parse)
  where
    operator = Left <$> Variable.parseOperator <|> Right <$> token ":"
    make position (Left operator) right left =
      Infix
        { left,
          operator = position Marked.:@ operator,
          right
        }
    make operatorPosition (Right ()) tail head =
      InfixCons
        { head,
          operatorPosition,
          tail
        }
    negate startPosition negative = Negate {startPosition, negative}

-- |
-- What is inside brackets and begins with an expression: either an
-- expression with operators in it, or the same with one more operator at
-- its end, which is a section. The two begin alike and are told apart only
-- at their end, so they are read together. Read one after the other, what
-- is inside brackets is read twice, and so four times inside two pairs, and
-- so on.
parseOpen :: Parser (Either (Expression Position) (Infix Position))
parseOpen =
  asum
    [ Right <$> (negated <$> position <*> (token "-" *> parse)),
      Expression.parse2 >>= go []
    ]
  where
    negated startPosition negative = Negate {startPosition, negative}
    operator = Left <$> Marked.parseOperator <|> Right <$> (position <* token ":")
    -- The operands and operators so far, the last first, and an operand.
    go before expression =
      optional operator >>= \case
        Nothing -> pure $ Right $ joined (reverse before) (Expression expression)
        Just operator ->
          asum
            [ -- What is negated runs to the end, and is no section.
              Right . joined (reverse ((expression, operator) : before))
                <$> (negated <$> position <*> (token "-" *> parse)),
              optional Expression.parse2 >>= \case
                Just next -> go ((expression, operator) : before) next
                Nothing -> case reverse ((expression, operator) : before) of
                  first : rest -> pure $ Left $ sectioned (first :| rest)
                  [] -> error "a section of nothing"
            ]
    joined [] final = final
    joined ((left, Left operator) : rest) final =
      Infix
        { left,
          operator,
          right = joined rest final
        }
    joined ((head, Right operatorPosition) : rest) final =
      InfixCons
        { head,
          operatorPosition,
          tail = joined rest final
        }

parseLeftSection :: Parser (Expression Position)
parseLeftSection = sectioned <$> some ((,) <$> Expression.parse2 <*> operator)
  where
    operator = Left <$> Marked.parseOperator <|> Right <$> (position <* token ":")

-- |
-- A section, of its operands each with the operator after it.
sectioned ::
  NonEmpty (Expression Position, Either (Marked.QualifiedName Position) Position) ->
  Expression Position
sectioned = parseLeftSection
  where
    parseLeftSection expressions = case snd $ NonEmpty.last expressions of
      Left operator ->
        Expression.LeftSection
          { leftSection = infixed expressions,
            operator
          }
      Right operatorPosition ->
        Expression.LeftSectionCons
          { leftSection = infixed expressions,
            operatorPosition
          }
    infixed ((expression, _) :| []) = Expression expression
    infixed ((left, Left operator) :| expression : expressions) =
      Infix
        { left,
          operator,
          right = infixed (expression :| expressions)
        }
    infixed ((head, Right operatorPosition) :| expression : expressions) =
      InfixCons
        { head,
          operatorPosition,
          tail = infixed (expression :| expressions)
        }
