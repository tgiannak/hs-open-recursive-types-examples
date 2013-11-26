Open Recursive Types Examples
=============================

These are short examples about how to use open recursive type definitions to
abstract the recursion out of data types, written up as a refresher for myself.
The technique comes in useful when one has a recursive data type that either
requires annotation or that has many recursive cases where defining the
recursive functions would be tedious.

First we set up the machinery necessary to do all of the work, then we show
some examples to make it clear.

Though `DeriveFunctor` is not necessary, it saves the tedium of writing the
obvious `Functor` declarations for our data types.

> {-# LANGUAGE DeriveFunctor #-}
> module OpenRecursiveTypesExamples where

> import Data.List (intercalate)


Open type definition
--------------------

Our examples will use a Propositional Logic formula data type, since it lets us
show many of the different interesting parts we need without being
distractingly complicated.

The convention we use for the name of the open form is to suffix with an `F`.

> data FormulaF formula
>     = Atom String
>     | Not formula
>     | And [formula]
>     | Or [formula]
>     | Implies formula formula
>     | Iff formula formula
>     deriving (Functor)


Closing the type definition with Fix
------------------------------------

In order to close the recursive type, we could write

````haskell
newtype Formula = FixedFormula (FormulaF Formula)
````

however, since we want this to be a general technique, we can factor out the
pattern.

> newtype Fix f = In { out :: (f (Fix f)) }

This makes the plain closed data type:

> type Formula = Fix FormulaF

Constructors
------------

Since the wrapping necessary to glue together constructors is unpleasant, it is
useful to define some constructors that do it for us. One common convention for
these constructors is to just lowercase the open constructor names, but in this
case we use the `mkCtor` convention, since otherwise we would conflict with
some Prelude names that we intend to use later.

> mkAtom :: String -> Formula
> mkAtom = In . Atom

> mkAnd :: [Formula] -> Formula
> mkAnd = In . And

> mkImplies :: Formula -> Formula -> Formula
> mkImplies f g = In $ Implies f g

...etc.

An example formula:

> example :: Formula
> example = mkAnd [mkAtom "p", mkAtom "q"]

The magic bit: cata
-------------------

This is the magic: we also pull the recursive part of our function definitions
into a reusable definition. For details:
http://www.haskell.org/haskellwiki/Catamorphisms

Think of it this way: the `Functor` definition encodes the recursion already, so
`cata` just makes use of that information via `fmap`. In other words, use `out`
to pull the real structure out of the `Fix` constructor, `fmap` down the
structure to handle the recursion, and then apply `g` at the top to finish it
off.

> cata :: (Functor f) => (f a -> a) -> Fix f -> a
> cata g = g . fmap (cata g) . out


Example: show
-------------

Define the function at just one layer...

> show' :: FormulaF String -> String
> show' f = case f of
>     Atom s -> s
>     Not s -> s
>     And xs -> "And [" ++ intercalate "," xs ++ "]"
>     Or xs  -> "Or [" ++ intercalate "," xs ++ "]"
>     Implies x y -> "(" ++ x ++ " -> " ++ y ++ ")"
>     Iff x y -> "(" ++ x ++ " <-> " ++ y ++ ")"

And then add the recursion in afterwards.

> showFormula = cata show'


Example eval
------------

The one-level definition has a result that is parameterized on the environment,
which is just a list of the true propositions. This works with First Order
Logic as well, since we can add to the environment if necessary as we evaluate
down the tree.

> type Env = [String]

> eval' :: FormulaF (Env -> Bool) -> Env -> Bool
> eval' fmla env = case fmla of
>     Atom s -> s `elem` env
>     Not x  -> not (x env)
>     And xs -> all ($ env) xs
>     Or xs -> any ($ env) xs
>     Implies x y -> y env || not (x env)
>     Iff x y -> (x env && y env) || (not (x env) && not (y env))

> eval :: Formula -> Env -> Bool
> eval = cata eval'


Alternative eval
----------------

If we want to be more concise, we can write the definition like this, but this
style doesn't work if we have to modify the environment when evaluating, like
we would for First Order Logic.

> eval2 :: Env -> Formula -> Bool
> eval2 env = cata $ \fmla -> case fmla of
>     Atom s -> s `elem` env
>     Not x  -> not x
>     And xs -> and xs
>     Or xs -> or xs
>     Implies x y -> y || not x
>     Iff x y -> (x && y) || (not x && not y)


Annotations
-----------

> data Loc = Loc { line :: Int, col :: Int }
>     deriving (Show)

Annotate it with a source location, but leave it open so that we can keep
adding new data.

> data LocAnnF a = LocAnnF Loc a
>    deriving (Functor)

We need a way to write down type composition. Unfortunately, we need to use a
newtype for it and pollute our code with the extra constructors, since a type
synonym will not let us drop arguments to talk about higher-kinded types.

Also, since we have the newtype, we also need to explicitly say that we can
`fmap` all the way down.

> newtype Compose g f a = Comp { unComp :: g (f a) }
>     deriving (Functor)

If you have the TypeOperators extension turned on, then the following
definition is usually nicer to read:

````haskell
newtype (g :. f) a = Comp { unComp :: g (f a) }
    deriving (Functor)
````

Since with that the open types can be written as `LocAnnF :. FormulaF`, which
looks like how we compose values `g . f`. This is especially useful if there is
a long chain of annotations that need to be added to a recursive type.

We can fix the composition of the open annotation and the open formula types to
get a closed, annotated formula type.

> type LocFormula = Fix (Compose LocAnnF FormulaF)

> mkLocAtom :: Loc -> String -> LocFormula
> mkLocAtom loc s = In . Comp $ LocAnnF loc (Atom s)

> mkLocAnd :: Loc -> [LocFormula] -> LocFormula
> mkLocAnd loc fs = In . Comp $ LocAnnF loc (And fs)

> mkLocImplies :: Loc -> LocFormula -> LocFormula -> LocFormula
> mkLocImplies loc f g = In . Comp $ LocAnnF loc (Implies f g)

> locExample :: LocFormula
> locExample = mkLocAnd (Loc 0 1) [mkLocAtom (Loc 0 4) "p", mkLocAtom (Loc 0 6) "q"]


Example: show with annotations
------------------------------

If we write something that handles the annotation on its own...

> showAnnF :: LocAnnF String -> String
> showAnnF (LocAnnF (Loc line col) s) = s ++ "@(" ++ show line ++ "," ++ show col ++ ")"

then we have to write our catamorphism by hand, since we need to fold two steps
at a time.

> showLocFormula :: LocFormula -> String
> showLocFormula = showAnnF . fmap (show' . fmap showLocFormula) . unComp . out

Example: atom occurrences
-------------------------

On the other hand, if we write something that handles one step of both
annotation and formula...

> findOccurrences' :: LocAnnF (FormulaF [(Loc, String)]) -> [(Loc, String)]
> findOccurrences' (LocAnnF loc fmla) = case fmla of
>     Atom s -> [(loc, s)]
>     Not  s -> s
>     And xs -> concat xs
>     Or  xs -> concat xs
>     Implies x y -> x ++ y
>     Iff x y -> x ++ y

... we can use cata again.

> findOccurrences = cata (findOccurrences' . unComp)

This gives us a choice between how we want our reusability: do we have lots of
annotated things, or do we want to write lots of recursive definitions on one
thing with different annotations.

Example: forget annotations
---------------------------

Also, we can project away the annotations rather easily.

> forgetLocs' :: LocAnnF (FormulaF Formula) -> Formula
> forgetLocs' (LocAnnF loc fmla) = In fmla

> forgetLocs :: LocFormula -> Formula
> forgetLocs = cata (forgetLocs' . unComp)
