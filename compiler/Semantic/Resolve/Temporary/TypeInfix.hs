module Semantic.Resolve.Temporary.TypeInfix where

import Data.Void (Void, absurd)
import qualified Semantic.Index.Constructor as Constructor (cons)
import qualified Semantic.Index.Type2 as Type2
import qualified Semantic.Resolve.Binding.Constructor as Constructor (Detail (..), get)
import Semantic.Resolve.Context
  ( Context (..),
    typeFixity,
    (!=~),
  )
import qualified Semantic.Resolve.Go.Type as Type (resolve)
import Semantic.Resolve.Temporary.Infix (Infix (..))
import qualified Semantic.Resolve.Temporary.Infix as Infix
import Semantic.Stage (Resolve)
import Semantic.Tree.Type (Synonym (..), Type)
import qualified Semantic.Tree.Type as Type
  ( Type
      ( Call,
        Constructor,
        argument,
        constructor,
        constructorPosition,
        function,
        startPosition,
        synonym
      ),
  )
import Syntax.Position (Position)
import Syntax.Tree.Fixity (Associativity (..), Fixity (..))
import Syntax.Tree.Marked (Marked ((:@)))
import qualified Syntax.Tree.Type as SyntaxType (Type (..))
import qualified Syntax.Tree.TypeInfix as Syntax (Infix (..))
import Prelude hiding (Either (Left, Right))

data Index scope
  = Constructor !Position !(Constructor.Detail scope)
  | Cons !Position
  | -- | A type operator: the type it names, and how it binds.
    Operator !Position !(Type Position Resolve scope) !Fixity

instance Infix.Token (Index scope) where
  position = \case
    Constructor position _ -> position
    Cons position -> position
    Operator position _ _ -> position
  fixity = \case
    Constructor _ Constructor.Binding {fixity} -> fixity
    Cons _ -> Fixity {associativity = Right, precedence = 5}
    Operator _ _ fixity -> fixity

fixWith ::
  Maybe Associativity ->
  Int ->
  Infix Void (Index scope) (Type Position Resolve scope) ->
  Type Position Resolve scope
fixWith = Infix.fixWith absurd operator
  where
    operator ::
      Type Position Resolve scope ->
      Index scope ->
      Type Position Resolve scope ->
      Type Position Resolve scope
    operator left operator right = case operator of
      Constructor constructorPosition Constructor.Binding {index} ->
        Type.Constructor
          { startPosition = Type.startPosition left,
            constructorPosition,
            constructor = Type2.Lifted index,
            synonym = NoSynonym
          }
          `call` left
          `call` right
      Operator _ typex _ -> typex `call` left `call` right
      Cons constructorPosition ->
        Type.Constructor
          { startPosition = Type.startPosition left,
            constructorPosition,
            constructor = Type2.Lifted Constructor.cons,
            synonym = NoSynonym
          }
          `call` left
          `call` right
      where
        call function argument =
          Type.Call
            { startPosition = Type.startPosition function,
              function,
              argument
            }

fix :: Infix Void (Index scope) (Type Position Resolve scope) -> Type Position Resolve scope
fix = fixWith Nothing 0

resolve :: Context scope -> Syntax.Infix Position -> Infix Void (Index scope) (Type Position Resolve scope)
resolve context = \case
  Syntax.Type type1 -> Single (Type.resolve context type1)
  Syntax.Infix {left, operator = operator@(operatorPosition :@ _), right} ->
    Infix
      (Type.resolve context left)
      (Constructor operatorPosition $ Constructor.get $ context !=~ operator)
      (resolve context right)
  Syntax.InfixCons {head, operatorPosition, tail} ->
    Infix (Type.resolve context head) (Cons operatorPosition) (resolve context tail)
  -- A type operator names a type, which is resolved as one written alone.
  -- It binds as its fixity says.
  Syntax.Operator {left, typeOperator = typeOperator@(operatorPosition :@ _), right} ->
    Infix
      (Type.resolve context left)
      (Operator operatorPosition (Type.resolve context (SyntaxType.Constructor {startPosition = operatorPosition, constructor = typeOperator})) (typeFixity context typeOperator))
      (resolve context right)
