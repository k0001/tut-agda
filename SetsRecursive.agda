module SetsRecursive where

-- # Recursive sets


open import Data.Bool using (Bool; true; false)
-- ^This opens and imports 'Data.Bool' from the standard library.
-- Without 'open' we could only refer to the functions imported with qualified
-- names such as 'Data.Bool.true'


-- ## Peano numbers representation

data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

-- The above definition yields the infinite set of statements:
--   ℕ : Set
--   zero : ℕ
--   suc zero : ℕ
--   suc (suc zero) : ℕ
--   suc (suc (suc zero)) : ℕ
--   … : ℕ

-- We may use 0,1,2,… instead of 'zero', 'suc zero', …
-- using this Agda magic declaration:

{-# BUILTIN NATURAL ℕ #-}
{-# BUILTIN ZERO    zero #-}
{-# BUILTIN SUC     suc #-}


-- ## Set element definitions

nine : ℕ
nine = suc (suc (suc (suc (suc (suc (suc (suc (suc zero))))))))

ten : ℕ
ten = suc nine



-- ## Binary representation of ℕ

data ℕ⁺ : Set where
  one      : ℕ⁺
  double   : ℕ⁺ → ℕ⁺
  double+1 : ℕ⁺ → ℕ⁺

-- ^The above definition of ℕ⁺ yields inifinitely, without ordering:
--  ℕ⁺ : Set
--  one : ℕ⁺
--  double one : ℕ⁺
--  double+1 one : ℕ⁺
--  double (double one) : ℕ⁺
--  double+1 (double one) : ℕ⁺
--  double (double+1 one) : ℕ⁺
--  double+1 (double+1 one) : ℕ⁺
--  … : ℕ⁺



data ℕ₂ : Set where
  zero : ℕ₂
  id   : ℕ⁺ → ℕ₂

-- ^The following definition yields infinitely:
--  ℕ₂ : Set
--  zero : ℕ₂
--  id one : ℕ₂
--  id (double one) : ℕ₂
--  id (double+1 one) : ℕ₂
--  id (double (double one)) : ℕ₂
--  id (double+1 (double one)) : ℕ₂
--  id (…)  : ℕ₂


nineℕ₂ : ℕ₂
nineℕ₂ = id (double+1 (double (double one)))


-- Soon we will prove in Agda that ℕ and ℕ₂ are isomorphic with the following
-- relation:
--   ℕ                      ℕ₂
--   zero                   zero
--   suc zero               id one
--   suc (suc zero)         id (double one)
--   suc (suc (suc zero))   id (double+1 one)


-- ## Rationale behind different representations
--
-- Different representations might be useful for different purposes.
-- A good strategy is to choose the right representation for each task and give
-- the isomorphisms between representations.
--
-- Question: ¿Which representation (ℕ or ℕ₂) is better for the following tasks?
--
--  *  Computing n * 2. ℕ₂
--  *  Computing ⌊n / 2⌋. ℕ₂
--  *  Deciding whether the number is odd. ℕ₂
--  *  Computing n + m. ℕ
--  *  Computing n * m. ℕ
--  *  Proving that n + m = m + n for all m and n. ℕ
--  *  Storing the number. ℕ₂


-- Excercise: Define ℤ and ℚ (several solutions are possible)

-- Is this OK?
data ℤ : Set where
  id  : ℕ  → ℤ
  neg : ℕ⁺ → ℤ

-- Is this OK?
data ℚ : Set where
  rational : ℤ → ℕ⁺ → ℚ -- what about negative denominator?



-- ## Binary trees

data BinTree : Set where
  leaf : BinTree
  node : BinTree → BinTree → BinTree

-- BinTree elements are good for representing binary trees (just the shapes
-- without data).

-- Excercise: define binary trees acording to the following shapes:

-- * z
btz : BinTree
btz = leaf

-- * o -> z1
--   o -> z2
bto = node leaf leaf

-- * x -> a
--   x -> b
--   a -> a1
--   a -> a2
--   b -> b1
--   b -> b2
btx = node (node leaf leaf) (node leaf leaf)

-- * x1 -> x2 -> x3 -> x4
--   x1 -> x1v
--   x2 -> x2v
--   x3 -> x3v
btx1 = node (node (node leaf leaf) leaf) leaf



-- ## Infix notation

data BinTree' : Set where
  x   : BinTree'
  _+_ : BinTree' -> BinTree' -> BinTree'

-- ^The above definition yields:
--  BinTree' : Set
--  x : BinTree'
--  x + x : BinTree'
--  (x + x) + x : BinTree'
--  x + (x + x) : BinTree'
--  (x + x) + (x + x) : BinTree'
--  … : BinTree'

-- Underscores in names like '_+_' denote the space for the operands.
-- Precedence can be given using 'infix', 'infixl', 'infixr

infixr 3 _+_
-- ^yields
--    BinTree' : Set
--    x : BinTree'
--    x + x : BinTree'
--    (x + x) + x : BinTree'
--    x + x + x : BinTree'
--    (x + x) + x + x : BinTree'
--    … : BinTree'
--  so '_*_' has right precedence.



-- Excerise: Define binary tree with natural number data attached to the leaf.
data BinTreeLℕ : Set where
  leaf : ℕ → BinTreeLℕ
  node : BinTreeLℕ → BinTreeLℕ → BinTreeLℕ

-- Excersise: Define binary tree with natural number data attachet to the nodes
data BinTreeNℕ : Set where
  leaf : BinTreeNℕ
  node : BinTreeNℕ → ℕ → BinTreeNℕ → BinTreeNℕ

-- Excersise: Define binary tree with Booleans in the nodes and Naturals in the
-- leafs
data BinTreeNBoolLℕ : Set where
  leaf : ℕ → BinTreeNBoolLℕ
  node : BinTreeNBoolLℕ → Bool → BinTreeNBoolLℕ → BinTreeNBoolLℕ


-- Exercise: Define the list of natural numbers. Use '_∷_' as constructor with
-- right precedence.
data Listℕ : Set where
  nil : Listℕ
  _∷_ : ℕ → Listℕ → Listℕ

-- Excersise: Define the non-empty list of natural numbers.
data NonEmptyListℕ : Set where
  last : ℕ → NonEmptyListℕ
  _∷_  : ℕ → NonEmptyListℕ → NonEmptyListℕ

-- Excercise: Define trees with nodes with finite children (0,1,2,…)!
-- NOTE: Maybe I don't understand the exercise, but if I do, then I don't
-- understand the purpose
data Tree₀ : Set where
  leaf : Tree₀

data Tree₁ : Set where
  leaf : Tree₁
  node : Tree₁ → Tree₁

data Tree₂ : Set where
  leaf : Tree₂
  node : Tree₂ → Tree₂ → Tree₂

data Tree₃ : Set where
  leaf : Tree₃
  node : Tree₃ → Tree₃ → Tree₃ → Tree₃



-- ## Mutual definitions

-- To allow mutual definitions one must declare any set before using it.
data L : Set
data M : Set

data L where  -- note that ': Set' is missing here and in 'data M' below.
  nil : L
  _∷_ : ℕ → M → L

data M where
  _∷_ : Bool → L → M


-- Excersise: What are the elements of 'L' and 'M'?
--   nil : L
--   Bool :: nil : M
--   ℕ :: Bool :: nil : L
--   Bool :: ℕ :: Bool :: nil : M
--   ℕ :: Bool :: ℕ :: Bool :: nil : L
--   …

-- Exercise: Define a small grammar!

-- | Grammar for ℤ constants, addition, multiplication and negation.
data Expr : Set where
  int : ℤ -> Expr
  neg : Expr -> Expr
  add : Expr -> Expr -> Expr
  mul : Expr -> Expr -> Expr

