module SetsEnumerated where

-- # Enumerated sets

data Bool : Set where
  true  : Bool
  false : Bool

-- ^Interpretation of the above 'Bool' datatype definition:
--   * 'Bool' is a 'Set'
--   * 'true' is a constructor of data type 'Bool'
--   * 'false' is a constructor of data type 'Bool'
--   * There is nothing else that is a 'Bool'
--   * 'true' and 'false' are different.


-- Other data type examples:

data Answer : Set where
  yes   : Answer
  no    : Answer
  maybe : Answer

data Quarter : Set where
  east  : Quarter
  west  : Quarter
  north : Quarter
  south : Quarter


-- ## Isomorphic Sets

-- Suppose we had another Boolean set defined:
data Bool' : Set where
  true'  : Bool'
  false' : Bool'

-- * 'Bool' and 'Bool'' are _definitionally_ different.
-- * 'Bool' and 'Bool'' are _isomorphic_.
--   * Two sets are isomorphic if there is an isomporphism between them.
-- * Both 'Bool'' and 'Bool'' may represent Booleans.


-- ## Special finite sets

-- We can define finite sets n≥0 elements.
-- There are two special cases for n=0 and n=1

data ⊥ : Set where -- there is no constructor

data ⊤ : Set where
  tt : ⊤   -- "tt" stands for "Trivially True"

-- * We'll use ⊥ as the empty set and ⊤ as the one-element set.
-- * If we define two empty sets they will be different, unlike in Set Theory.


-- ## Syntactic abbreviation

-- If you have multiple elements of the same set you can define these in one
-- line:

data name : Set where
  elem1 elem2 elem3 : name








