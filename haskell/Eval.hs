module Eval (vars, eval) where

import Prelude hiding (pi)

import Expr

import Data.Map (Map, (!))
import Data.Map qualified as Map

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Maybe (fromMaybe)
import Text.Printf (IsChar(toChar))
import GHC.Base (VecElem(Int16ElemRep))

type Env = Map String Double

---------------------------------------------------------------------------
-- Type classes and class instances

instance Num Expr where
  fromInteger :: Integer -> Expr
  fromInteger = Val . fromIntegral

  negate :: Expr -> Expr
  negate (Val 0) = Val 0
  negate e       = Pre Neg e

  (+) :: Expr -> Expr -> Expr
  e + Val 0 = e 
  Val 0 + e = e 
  e1 + e2   = Bin Add e1 e2

  (*) :: Expr -> Expr -> Expr
  e     * Val 1 = e 
  Val 1 * e     = e
  e     * Val 0 = Val 0
  Val 0 * e     = Val 0
  e1    * e2    = Bin Mul e1 e2

instance Fractional Expr where
  fromRational :: Rational -> Expr
  fromRational = Val . fromRational

  (/) :: Expr -> Expr -> Expr
  (Val 0) / e = Val 0 
  e / Val 1   = Val 1
  e1 / e2     = Bin Div e1 e2

instance Floating Expr where
  sin, cos, log, exp :: Expr -> Expr
  sin = Pre Sin
  cos = Pre Cos
  log = Pre Log
  exp = Pre Exp

---------------------------------------------------------------------------
-- Maps for expression evaluation

preOpMap :: Map PreOp (Double -> Double)
preOpMap = Map.fromList [(Neg, negate), (Sin, sin), (Log, log), (Cos, cos), (Exp, exp)]

binOpMap :: Map BinOp (Double -> Double -> Double)
binOpMap = Map.fromList [(Add, (+)), (Mul, (*)), (Div, (/))]

preOpStrMap :: Map PreOp String
preOpStrMap = Map.fromList [(Neg, "-"), (Sin, "sin"), (Log, "log"), (Cos, "cos"), (Exp, "exp")]

binOpStrMap :: Map BinOp String
binOpStrMap = Map.fromList [(Add, "+"), (Mul, "*"), (Div, "/")]


{-|
Computes the set of variable names that appear within the given expression.
-}
vars :: Expr -> Set String
vars (Val _) = Set.empty
vars (Id x) = Set.singleton x
vars (Pre op e) = vars e
vars (Bin op e1 e2) = Set.union (vars e1) (vars e2)

{-|
Evaluates a given expression, evaluating any variables to their value within
the provided environment.
-}
eval :: Env -> Expr -> Double
eval env (Val n)        = n
eval env (Id var)       = env Map.! var
eval env (Pre op e)     = (preOpMap ! op) (eval env e)
eval env (Bin op e1 e2) = (binOpMap ! op) (eval env e1) (eval env e2)

-- what happens if we call eval Map.empty (Id "x")?
-- Let's try and define eval with Maybes.

eval1 :: Env -> Expr -> Maybe Double
eval1 env (Val n)        = Just n
eval1 env (Id var)       = Map.lookup var env
eval1 env (Pre op e)     = 
  case eval1 env e of 
    Nothing -> Nothing
    Just e' -> Just $ (preOpMap ! op) e'
eval1 env (Bin op e1 e2) = 
  case eval1 env e1 of
    Nothing  -> Nothing 
    Just e1' -> case eval1 env e2 of
      Nothing -> Nothing 
      Just e2' -> Just $ (binOpMap ! op) e1' e2'

{-
Let's look at this line: 
Just e' -> Just $ (preOpMap Map.! op) e'
The overall structure here is Just x -> Just f x
This is just an fmap!
-}

eval2 :: Env -> Expr -> Maybe Double
eval2 env (Val n)        = Just n
eval2 env (Id var)       = Map.lookup var env
eval2 env (Pre op e)     = preOpMap ! op <$> eval2 env e
eval2 env (Bin op e1 e2) = 
  case eval2 env e1 of
    Nothing  -> Nothing 
    Just e1' -> (binOpMap ! op) e1' <$> eval2 env e2

{-
How are we going to handle that first operand e1?

Let's say we started with something like this: (binOpMap Map.! op) <$> eval env e1
This is what we had above. The only difference is that (binOpMap Map.! op) expects two parameters,
so we end up with a partially applied function wrapped up in a Maybe.

Typically, an fmap has this signature: <*> :: (a -> b) -> Maybe a -> Maybe b.

However, (binOpMap Map.! op) has type (a -> (b -> c)), so we end up with something like this:
(a -> (b -> c)) -> Maybe a -> Maybe (b -> c)

So, we've established that: 
 (binOpMap Map.! op) <$> eval env e1 is going to give us a Maybe (b -> c)

hmm we want some way of applying the function INSIDE the maybe to the (eval2 env e2)
? :: Maybe (b -> c) -> Maybe b -> Maybe c

well this is just the definition of an applicative map <*> isn't it...

(binOpMap Map.! op) <$> eval env e1 <*> eval env e2 should do the trick
-}

eval3 :: Env -> Expr -> Maybe Double
eval3 env (Val n)        = Just n
eval3 env (Id var)       = Map.lookup var env
eval3 env (Pre op e)     = preOpMap ! op <$> eval3 env e
eval3 env (Bin op e1 e2) = (binOpMap ! op <$> eval3 env e1) <*> eval3 env e2


{- 
Next, I want to demonstrate how we can do the same thing with monads. 

Look at the 'Pre' case. First, we evaluate (eval4 env e), and we 'feed the result' into the
lambda (\e' -> Just $ (preOpMap ! op) e'). We can think of this lambda as just another 'line of code'
that is aware of the previous operation (evaluating e). 

For BinOp, we just nest two lambdas, so that the final "(\e2' ->  Just $ (binOpMap ! op) e1' e2')" has access
to the results of both the evaluated e1 and e2
-}

eval4 :: Env -> Expr -> Maybe Double
eval4 env (Val n)        = Just n
eval4 env (Id var)       = Map.lookup var env
eval4 env (Pre op e)     = (eval4 env e) >>= (\e' -> Just $ (preOpMap ! op) e')
eval4 env (Bin op e1 e2) = 
  (eval4 env e1) >>= (\e1' -> 
    (eval4 env e2) >>= (\e2' ->  Just $ (binOpMap ! op) e1' e2'))

{-
The following code is semantically equivalent to eval4, just with the 'do' notation.
Compare each line to its respective line in eval4 to properly understand what the Monads are being used for.
-}

eval5 :: Env -> Expr -> Maybe Double
eval5 env (Val n)  = Just n
eval5 env (Id var) = Map.lookup var env
eval5 env (Pre op e) = do 
  e' <- eval5 env e
  Just $ (preOpMap ! op) e'
eval5 env (Bin op e1 e2) = do 
  e1' <- eval5 env e1
  e2' <- eval5 env e2
  Just $ (binOpMap ! op) e1' e2'
