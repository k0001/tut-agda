module Term_Inference where

open import Data.Empty    using (⊥)
open import Data.Unit     using (⊤; tt)
open import Data.Sum      using (_⊎_; inj₁; inj₂)
open import Data.Nat      using (ℕ; zero; suc)

-- The Agda compiler tries to infer terms marked with underscore.
-- If the choice of term is ambiguous, term inference fails.

data Fin′ : ℕ → Set where
  zero : (n : _) → Fin′ (suc n)          -- ℕ is inferred
  suc  : (n : _) → Fin′ n → Fin′ (suc n) -- ℕ is inferred.

x′ : Fin′ 3
x′ = suc _ (zero _) -- 2 and 1 are inferred


-- # Implicit arguments
-- Underscores can be hidden: Mark argument positions of constructors implicity
-- with curly brackets

data Fin : ℕ → Set where
  zero : {n : _} → Fin (suc n)          -- ℕ is inferred
  suc  : {n : _} → Fin n → Fin (suc n) -- ℕ is inferred.

x : Fin 3
x = suc {_} (zero {_})

-- {_} can be deleted

x2 : Fin 3
x2 = suc zero

-- Although `zero : Fin 1` and `zero : Fin 2`, `zero` does not have multiple
-- different types; the implicit arguments make the types unique.


-- # Syntactic abbreviations
-- Variables with inferred types can be introduced with ∀

data Fin′₂ : ℕ → Set where
  zero : ∀ n → Fin′₂ (suc n)
  suc  : ∀ n → Fin′₂ n → Fin′₂ (suc n)

data Fin₂ : ℕ → Set where
  zero : ∀ {n} → Fin₂ (suc n)
  suc  : ∀ {n} → Fin₂ n → Fin₂ (suc n)
