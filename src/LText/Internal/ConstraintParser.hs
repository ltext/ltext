module LText.Internal.ConstraintParser where


import Data.String.Utils (startswith)

startsWith :: Eq a => [a] -> [a] -> Bool
startsWith = flip startswith


data Symbol = OpenParen
            | CloseParen
            | DoubleArrow
            | Word {val :: String}
            | Product
            | Union
            | Constr Constraint
              deriving (Show, Eq)

lexify :: String -> [Symbol]
lexify "" = []
lexify s | s `startsWith` "("  = OpenParen : (lexify $ drop 1 s)
         | s `startsWith` ")"  = CloseParen : (lexify $ drop 1 s)
         | s `startsWith` "=>" = DoubleArrow : (lexify $ drop 2 s)
         | s `startsWith` "*" ||
           s `startsWith` "&" ||
           s `startsWith` ","  = Product : (lexify $ drop 1 s)
         | s `startsWith` "+" ||
           s `startsWith` "|"  = Union : (lexify $ drop 1 s)
         | s `startsWith` " "  = lexify $ drop 1 s
         | otherwise = let (n,word) = grabTil ['(',')','='
                                              ,'.','*','&'
                                              ,',','+','|',' '] s in
                           (Word word) : (lexify $ drop n s)


data ConstMode = Only | Without
  deriving (Show, Eq)
data Constraint = Constraint {var :: String, ext :: String, mode :: ConstMode}
  deriving (Show, Eq)

lexConstr :: String -> Maybe Constraint
lexConstr s = foldr -- todo
-- accumulator is a maybe? Fuckin... monadic?? `sequence`?


fixWords :: [Symbol] -> [Symbol]
fixWords [] = []
fixWords ((Word n):xs) = -- todo
