module SetsIndexed where

open import Data.Empty    using (⊥)
open import Data.Unit     using (⊤; tt)
open import Data.Sum      using (_⊎_; inj₁; inj₂)
open import Data.Bool     using (Bool; true; false)
open import Data.Nat      using (ℕ; zero; suc)


-- # Fin, family of finite sets

-- We wish to define a ℕ-indexed family of sets 'Fin' such that 'Fin n' has
-- exactly 'n ' elements.  Given the definition of 'Fin', the following
-- equivalences would hold:
--
-- +-----+----------------------------------------------------------------+
-- |0    |Fin 0 ~ ⊥                                                       |
-- +-----+----------------------------------------------------------------+
-- |1    |Fin 1 ~ ⊤ ~ Maybe ⊥ ~ ⊤ ⊎ ⊥                                     |
-- +-----+----------------------------------------------------------------+
-- |2    |Fin 2 ~ Bool ~ Maybe ⊤ ~ Maybe (Maybe ⊥) ~ ⊤ ⊎ ⊤ ⊎ ⊥            |
-- +-----+----------------------------------------------------------------+
-- |3    |Fin 3 ~ Maybe Bool ~ Maybe (Maybe (Maybe ⊥)) ~ ⊤ ⊎ ⊤ ⊎ ⊤ ⊎ ⊥    |
-- +-----+----------------------------------------------------------------+
-- |…    |…                                                               |
-- +-----+----------------------------------------------------------------+

-- Fin is a set indexed with a natural number
data Fin : ℕ → Set where
  zero : (n : ℕ) → Fin (suc n)
  suc  : (n : ℕ) → Fin n → Fin (suc n)

-- the above definition yields:
--   zero 0 : Fin 1
--   zero 1 : Fin 2
--   zero 2 : Fin 3
--   …
--   suc 1 (zero 0) : Fin 2
--   suc 2 (zero 1) : Fin 3
--   suc 3 (zero 2) : Fin 4
--   …
--   suc 2 (suc 1 (zero 0)) : Fin 3
--   suc 3 (suc 2 (zero 1)) : Fin 4
--   suc 4 (suc 3 (zero 2)) : Fin 5
--   …
-- which can be rearranged as
--   zero 0 : Fin 1
--
--   zero 1 : Fin 2
--   suc 1 (zero 0) : Fin 2
--
--   zero 2 : Fin 3
--   suc 2 (zero 1) : Fin 3
--   suc 2 (suc 1 (zero 0)) : Fin 3
--
--   zero 3 : Fin 4
--   suc 3 (zero 2) : Fin 4
--   suc 3 (suc 2 (zero 1)) : Fin 4
--   suc 3 (suc 2 (suc 1 (zero 0))) : Fin 4
--
-- so we can conclude that 'Fin n' has 'n' distinct elements.

-- Excercise: Define a 'Bool' indexed family of sets such that the set indexed
--   by false contains no elelements and the set indexed by true contains one
--   element.
data FinBool : Bool → Set where
  false : ⊥ → FinBool false
  true  : ⊤ → FinBool true

-- Excersise: Define a ℕ indexed family of sets such that the sets indexed by
-- even numbers contains one element and other are empty
even : ℕ → Set
even zero = ⊤
even (suc zero) = ⊥
even (suc (suc n)) = even n

data FinEven : ℕ → Set where
  feven : (n : ℕ) {_ : even n} → FinEven n

feven-ok = feven 2
-- feven-bad = feven 3


-- ## Vec

-- 'Vec A n' is a 'n'-tuple of elements of 'A'.

data Vec (A : Set) : ℕ → Set where
  []  : Vec A zero
  _∷_ : A → {n : ℕ} → Vec A n → Vec A (suc n)

v1 : Vec Bool 1
v1 = true ∷ []

v2 : Vec Bool 2
v2 = false ∷ (true ∷ [])

-- Excersise: Define a 'Bool' indexed family of sets with two parameters, 'A'
-- and 'B', such that the set indexed by 'false' contains an 'A' element and the
-- set indexed by 'true' contains a 'B' element.
data FinBool₂ (A B : Set) : Bool → Set where
  finb₁ : A → FinBool₂ A B false
  finb₂ : B → FinBool₂ A B true
