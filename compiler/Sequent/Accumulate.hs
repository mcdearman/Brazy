-- |
-- A function that calls itself and adds a number to what the call gives
-- back, as @collatz n = 1 + collatz m@ does, is given what it has added so
-- far instead, and calls itself last.
--
-- There is no stack: what is to be done with what a call gives back is an
-- object made before the call, a consumer that adds the number and hands
-- the sum on. So @collatz@ made one for each step it took. Adding @Int@s is
-- associative, wrapping round or not, so the sum can be worked out on the
-- way down: a copy of the function takes what has been added so far as a
-- parameter more, adds the number to it where it called itself, and calls
-- itself last with that; where it gives a value back it gives back the sum
-- of the two. The function itself calls the copy with nothing added. The
-- copy is one of the function's family, made wherever the function is.
--
-- Everything the function gives back is an @Int@, since what it gives back
-- where it calls itself is added as one.
module Sequent.Accumulate (accumulate) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Generate.Global as Global (Global (..))
import Sequent.Primitive (Primitive (..))
import qualified Sequent.Tree as Literal (Literal (..))
import Sequent.Tree
  ( Alternative (..),
    Binder (..),
    Consumer (..),
    Producer (..),
    Statement (..),
    Strategy (..),
    free,
    separator,
  )

-- |
-- The function as it is to be, calling its copy, and the copy, if it adds
-- a number to what it gives back where it calls itself.
accumulate :: Global.Global -> Producer -> Maybe (Producer, (Global.Global, Producer))
accumulate self = \case
  Function parameters result body
    | adds self result body,
      Set.null (own `Set.intersection` (names body <> Set.fromList [name | Binder {name} <- parameters])) ->
        let copy = self {Global.name = Text.snoc (Global.name self) separator <> Text.pack "accumulated"}
            start = Function parameters result $ Cut (Global copy) (Apply (Literal (Literal.Int 0) : [Variable name | Binder {name} <- parameters]) (Covariable result))
            worker = Function (Binder {name = sum', strategy = ByValue} : parameters) result (rewrite self copy result body)
         in Just (start, (copy, worker))
  _ -> Nothing
  where
    sum' = Text.pack "sum%"
    own = Set.fromList (map Text.pack ["sum%", "sum%next", "sum%given"])

-- |
-- Where a function calls itself with every argument and adds a number to
-- what the call gives back before giving it back: the arguments, and the
-- number.
added :: Global.Global -> Text -> Statement -> Maybe ([Producer], Producer)
added self result = \case
  Cut (Global called) (Apply arguments (MuTilde Binder {name = given, strategy = ByValue} (Primitive IntAdd [left, right] (Covariable to))))
    | called == self,
      to == result,
      Just number <- other given left right,
      Set.notMember given (free number) ->
        Just (arguments, number)
  _ -> Nothing
  where
    other given left right
      | Variable name <- left, name == given, atom right = Just right
      | Variable name <- right, name == given, atom left = Just left
      | otherwise = Nothing

atom :: Producer -> Bool
atom = \case
  Variable _ -> True
  Literal (Literal.Int _) -> True
  _ -> False

-- |
-- Whether a function adds a number to what calling itself gives back
-- anywhere, and everything else it gives back is plainly an @Int@: a number
-- written there, what a primitive of @Int@s gives, or what calling itself
-- last gives. What a function of a GADT gives back may be of another type
-- where it does not call itself, as @eval :: Expr a -> a@'s is.
adds :: Global.Global -> Text -> Statement -> Bool
adds self result body = any (const True) (sites body) && ints body
  where
    sites = statement
    statement value = case added self result value of
      Just _ -> [()]
      Nothing -> case value of
        Cut left right -> producer left ++ consumer right
        Primitive _ arguments next -> concatMap producer arguments ++ consumer next
        LetRec bindings inner -> concatMap (producer . snd) bindings ++ statement inner
        _ -> []
    producer = \case
      Constructor _ fields -> concatMap producer fields
      Mu _ inner -> statement inner
      Function _ _ inner -> statement inner
      Thunk _ inner -> statement inner
      _ -> []
    consumer = \case
      MuTilde _ inner -> statement inner
      Case alternatives otherwise -> concat [statement inner | Alternative _ _ inner <- alternatives] ++ maybe [] statement otherwise
      Apply arguments next -> concatMap producer arguments ++ consumer next
      Force next -> consumer next
      Covariable _ -> []
    -- Whether all that is given to the consumer is plainly an Int.
    ints value = case added self result value of
      Just (arguments, _) -> all intsIn arguments
      Nothing -> case value of
        Cut (Global called) (Apply arguments (Covariable to))
          | called == self, to == result -> all intsIn arguments
        Cut (Literal (Literal.Int _)) (Covariable to) | to == result -> True
        Primitive primitive arguments (Covariable to)
          | to == result -> primitive `elem` integral && all intsIn arguments
        Cut left right -> intsIn left && intsOut right
        Primitive _ arguments next -> all intsIn arguments && intsOut next
        LetRec bindings inner -> all (intsIn . snd) bindings && ints inner
        _ -> True
    intsIn = \case
      Constructor _ fields -> all intsIn fields
      Mu _ inner -> ints inner
      Function _ _ inner -> ints inner
      Thunk _ inner -> ints inner
      _ -> True
    intsOut = \case
      Covariable to -> to /= result
      MuTilde _ inner -> ints inner
      Case alternatives otherwise -> and [ints inner | Alternative _ _ inner <- alternatives] && all ints otherwise
      Apply arguments next -> all intsIn arguments && intsOut next
      Force next -> intsOut next
    integral = [IntAdd, IntMinus, IntMultiply, IntNegate, IntAbs, IntSignum, IntQuot, IntRem, IntDiv, IntMod, IntegerCastToInt, IntegerTruncateToInt]

-- |
-- The copy's body: where it called itself and added a number, it adds the
-- number to the sum and calls the copy last; where it called itself last,
-- it calls the copy with the sum; and whatever else is given to its
-- consumer is given with the sum added.
rewrite :: Global.Global -> Global.Global -> Text -> Statement -> Statement
rewrite self copy result = statement
  where
    sum' = Variable (Text.pack "sum%")
    statement value = case added self result value of
      Just (arguments, number) ->
        Primitive IntAdd [sum', number] $
          MuTilde Binder {name = Text.pack "sum%next", strategy = ByValue} $
            Cut (Global copy) (Apply (Variable (Text.pack "sum%next") : map producer arguments) (Covariable result))
      Nothing -> case value of
        Cut (Global called) (Apply arguments (Covariable to))
          | called == self,
            to == result ->
              Cut (Global copy) (Apply (sum' : map producer arguments) (Covariable result))
        Cut left right -> Cut (producer left) (consumer right)
        -- What a primitive gives is a value, and is added as it is.
        Primitive primitive arguments (Covariable to)
          | to == result -> Primitive primitive (map producer arguments) plus
        Primitive primitive arguments next -> Primitive primitive (map producer arguments) (consumer next)
        LetRec bindings inner -> LetRec [(name, producer bound) | (name, bound) <- bindings] (statement inner)
        other -> other
    plus =
      MuTilde Binder {name = Text.pack "sum%given", strategy = ByValue} $
        Primitive IntAdd [sum', Variable (Text.pack "sum%given")] (Covariable result)
    producer = \case
      Constructor tag fields -> Constructor tag (map producer fields)
      Mu covariable inner -> Mu covariable (statement inner)
      Function parameters covariable inner -> Function parameters covariable (statement inner)
      Thunk covariable inner -> Thunk covariable (statement inner)
      other -> other
    consumer = \case
      Covariable to
        | to == result -> Force plus
      MuTilde binder inner -> MuTilde binder (statement inner)
      Case alternatives otherwise ->
        Case [Alternative tag binders (statement inner) | Alternative tag binders inner <- alternatives] (statement <$> otherwise)
      Apply arguments next -> Apply (map producer arguments) (consumer next)
      Force next -> Force (consumer next)
      other -> other

-- |
-- Every name something has, bound or used.
names :: Statement -> Set Text
names = statement
  where
    statement = \case
      Cut left right -> producer left <> consumer right
      Primitive _ arguments next -> foldMap producer arguments <> consumer next
      LetRec bindings inner -> Set.fromList (map fst bindings) <> foldMap (producer . snd) bindings <> statement inner
      _ -> Set.empty
    producer = \case
      Variable name -> Set.singleton name
      Constructor _ fields -> foldMap producer fields
      Mu covariable inner -> Set.insert covariable (statement inner)
      Function parameters covariable inner -> Set.insert covariable (Set.fromList [name | Binder {name} <- parameters] <> statement inner)
      Thunk covariable inner -> Set.insert covariable (statement inner)
      _ -> Set.empty
    consumer = \case
      Covariable name -> Set.singleton name
      MuTilde Binder {name} inner -> Set.insert name (statement inner)
      Case alternatives otherwise ->
        mconcat [Set.fromList [name | Binder {name} <- binders] <> statement inner | Alternative _ binders inner <- alternatives]
          <> foldMap statement otherwise
      Apply arguments next -> foldMap producer arguments <> consumer next
      Force next -> consumer next
