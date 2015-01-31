module LText.ATLC.Types where

-- | An flat term name
type Term = String

-- | All term expressions present after all applications to visible lambdas
-- are reduced
data Result = RTerm Term
            | Apply Expr Expr
            | Append Expr Expr
            | Literal
  deriving (Show, Eq)

-- | All term expressions calculable
data Expr = Lambda Term Expr
          | Result
  deriving (Show, Eq)

-- | A type variable
type Var = String

-- | Traditional natural number
data Nat = Z | S Nat
  deriving (Show, Eq)

-- | A @forall a b c...@ of type variables
type Decl = [Var]

-- | A @a >= 1@ constraint
type Ineq = (Var, Nat)

-- | The head of a type signature
data TypeScheme = TypeScheme Decl [Ineq]
  deriving (Show, Eq)

-- | Arithmetic with type variables and literals. Note that we only decrement
-- literals, because they are decremented by 1 only under (finite) term
-- application - we can be confident that no reduction of variant size will
-- happen.
data TypeExpr = Nat
              | Add TypeExpr TypeExpr
              | Sub TypeExpr Nat
  deriving (Show, Eq)

-- | A complete type signature, for terms and literal (or constant literal)
-- expressions.
data TypeSig = TypeTerm TypeScheme TypeExpr
             | TypeLit Nat
  deriving (Show, Eq)

-- | The facilitated parameter listing -> result. This is what we need to unify.
data Substitution = Substitution [Term] Result
  deriving (Show, Eq)
