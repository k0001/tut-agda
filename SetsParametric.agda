
module SetsParametric where

open import Data.Nat
open import SetsEnumerated using (⊤; tt; Bool; true; false)


data List (A : Set) : Set where
  []   : List A
  _∷_ : A → List A → List A

infixr 5 _∷_

-- ^Interpretation: 'List A' ∈ 'Set', where 'A' ∈ 'Set'. We call 'A' a parameter
--  of List and we can refer to 'A' in the definition of the set elements.
--
-- Example: Elements of 'List Bool'
--
--  [] : List Bool
--  true ∷ [] : List Bool
--  false ∷ [] : List Bool
--  true ∷ true [] : List Bool
--  false ∷ true [] : List Bool
--  …

-- Exercise: What is the connection betwen 'List ⊤' and ℕ?
--  []    ∼ zero
--  tt∷b ∼ suc b

-- Exercise: Define a 'Maybe' set.

data Maybe (A : Set) : Set where
  Just    : A → Maybe A
  Nothing : Maybe A

-- Exersise Define parametric trees (various sorts).

data BinTree (A : Set) : Set where
  leaf : A → BinTree A
  node : BinTree A → A → BinTree A → BinTree A



-- # Cartesian product

data _×_ (A B : Set) : Set where
  _,_ : A → B → A × B

infixr 4 _,_

-- @(A B : Set)@ is the way of specifying a set that parameterised by two sets.

-- Example: Elements of @Bool × Bool@
-- (the extra space is needed before the comma)
--   true , true
--   true , false
--   false , true
--   false , false


-- Exercise: How many elements are there in:
--  - ⊤ × ⊤: 1 * 1 = 1
--  - ⊤ × ⊥: 1 * 0 = 0
--  - ⊥ × ⊤: 0 * 1 = 0
--  - ⊥ × ⊥: 0 * 0 = 0


-- Exercise: How should we define 'Top' so that @∀ A : Set@. 'Top × A' would be
-- isomorphic to A (neutral element of _×_)?
-- Answer: ⊤?



-- # Disjoint Union (Sum)

data _⊎_ (A B : Set) : Set where
  inj₁ : A → A ⊎ B
  inj₂ : B → A ⊎ B

infixr 1 _⊎_

-- Exercices:
--
-- * What are the elements of 'Bool ⊎ ⊤'?
--    inj₁ true
--    inj₁ false
--    inj₂ tt
--
-- * What are the elements of '⊤ ⊎ (⊤ ⊎ ⊤)'?
--    inj₁ tt
--    inj₂ (inj₁ tt)
--    inj₂ (inj₁ tt)
--
-- * Name an already learned isomorphic type to '⊤ ⊎ ⊤'.
--    Bool.
--
-- * How should we define Bottom so that '∀ A : Set'. 'Bottom ⊎ A' would be
--   isomorphic to 'A' (Neutral element of _⊎_)?
--    ⊥
--
-- * Give an isomorphic definition of 'Maybe A' with the help of _⊎_ and ⊤
--    Just A  : Maybe A ∼ inj₁ tt : ⊤ ⊎ A
--    Nothing : Maybe A ∼ inj₂ a  : ⊤ ⊎ A


-- ## Mutually recursive sets
--
-- List₁ and List₂ are mutually recursive parametric sets:

data List₁ (A B : Set) : Set
data List₂ (A B : Set) : Set

data List₁ (A B : Set) where
  []  :                 List₁ A B
  _∷_ : A → List₂ A B → List₁ A B

data List₂ (A B : Set) where
  _∷_ : B → List₁ A B → List₂ A B

-- The smallest first 5 elements of List₁ ⊤ Bool:
--   [] : List₁ ⊤ Bool
--   true ∷ [] : List₁ ⊤ Bool
--   false ∷ [] : List₁ ⊤ Bool
--   tt ∷ true ∷ [] : List₁ ⊤ Bool
--   tt ∷ false ∷ [] : List₁ ⊤ Bool


-- ## Non-regular recursive set

data AlterList (A B : Set) : Set where
  []  :                     AlterList A B
  _∷_ : A → AlterList B A → AlterList A B

-- The smallest first 4 (+4) elements of 'AlterList ⊤ Bool'
-- []
-- true ∷ []
-- false ∷ []
-- tt ∷ true ∷ []
-- tt ∷ false ∷ []

-- ## Nested set

-- 'Square', the set of square matrices with 2ⁿ rows, is nested, because at
-- least one of its constructors refers to the set defined with more complex
-- parameter(s):

data T4 (A : Set) : Set where
  quad : A → A → A → A → T4 A

data Square (A : Set) : Set where
  zero :            A  → Square A  -- 2⁰ rows
  suc  : Square (T4 A) → Square A  -- 2ⁿ⁺¹ rows

x : Square ℕ
x = suc (suc (zero (quad (quad 1   2  3  4)
                         (quad 5   6  7  8)
                         (quad 9  10 11 12)
                         (quad 13 14 15 16))))

