{-# LANGAGUE GeneralizedNewtypeDeriving #-}
module LText.Internal.Types where


import LText.Internal.Utils (wrapTerms)
import LText.Internal.TypeSigParser.ErrM (Err (..))
import LText.Internal.TypeSigParser.PartypeSig (pExpr, myLexer)
import LText.Internal.TypeSigParser.AbstypeSig (Expr (..))


-- | Identifiers for type variables
newtype Label = Label {label :: String}
  deriving (Eq)
instance Show Label where
  show (Label s) = s

-- | Concrete Terms of a Type Signature
data SmallType = Unit
               | Var Label
                 deriving (Show, Eq)

-- | Type Signature AST
data Type = Flat SmallType
          | Arity Type Type
            deriving (Eq)
instance Show Type where
  show (Flat x) = show x
  show (Arity (Flat x) y) = show x ++ " -> " ++ show y
  show (Arity x y) = paren (show x) ++ " -> " ++ show y
    where
      paren a = ('(': a ) ++ ")"

-- | TODO: Create Read instance for Type
readType :: String -> Type
readType s = let (Ok ast) = parseToAst s in
                 astToType ast

-- | File Type Extensions
newtype FileExt = FileExt {fileExt :: String}
  deriving (Show, Eq)

-- | Qualifiers
data Constraint a = Pure a
                  | Only a FileExt
                  | Without a FileExt
                  | Or (Constraint a) (Constraint a)
                  | And (Constraint a) (Constraint a)
                    deriving (Show, Eq)


-- | Convenience function for turning a string into the BNFC AST
parseToAst :: String -> Err Expr
parseToAst = pExpr . myLexer . wrapTerms

-- | Convenience function for turning a BNFC AST to a Type
astToType :: Expr -> Type
astToType (EArrow x y) = Arity (astToType x) (astToType y)
astToType EUnit        = Flat Unit
astToType (ETerm l)    = Flat (Var (Label l))
