module LText.Internal.Utils where


import LText.Internal.Types (Type (..))

import Control.Monad.State
import Data.String.Utils (startswith)

-- | All symbols for type signatures. TODO: Support Qualifiers
data Symbol = Variable {identifier :: String}
            | Content
            | Arrow
            | OpenParen
            | CloseParen
              deriving (Show, Eq)

-- | Translate the input string into a list of symbols
lexify :: String -> [Symbol]
lexify "" = []
lexify s | s `startsWith` "->" = Arrow : (lexify $ drop 2 s)
         | s `startsWith` "(" = OpenParen : (lexify $ drop 1 s)
         | s `startsWith` ")" = CloseParen : (lexify $ drop 1 s)
         | s `startsWith` " " = lexify $ drop 1 s
         | s `startsWith` "Content" = Content : (lexify $ drop 7 s)
         | otherwise = let (sym, leftover) = getTerm s
                       in sym : (lexify leftover)
  where
    getTerm :: String -> (Symbol, String)
    getTerm x = let (term, n) = x `grabTil` ['-','(',')',' ']
                in (Variable term, drop n x)

    grabTil :: String -> [Char] -> (String, Int)
    string `grabTil` chars = let t = takeWhile (\z -> and $ map (z /=) chars) string
                             in (t, length t)
    
    startsWith = flip startswith



data Object = Unit | Term {ident :: String} | Expr
data Expr = Object
          | Arrow Object Object

-- | The parsing state is either a failure or a partial AST and leftovers
type ParseState = Either String (Type, String)

-- | Convenience function for extracting a successful result
getResult :: ParseState -> Type
getResult (Right (t, _)) = t


stage :: State Type String
stage = undefined
