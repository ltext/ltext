{-# LANGAGUE GeneralizedNewtypeDeriving #-}

module LText.Internal.Types where


-- | Identifiers for type variables
newtype Label = Label {label :: String}
  deriving (Eq)
instance Show Label where
  show (Label s) = s

-- | Concrete Terms of a Type Signature
data SmallType = Unit
               | Variant Label
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
      paren x = ('(': x ) ++ ")"


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
