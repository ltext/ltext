module LText.Internal.Utils where


import LText.Internal.TypeSigParser.PartypeSig (pExpr, myLexer)
import LText.Internal.TypeSigParser.AbstypeSig (Expr)
import LText.Internal.TypeSigParser.ErrM (Err)

import Data.String.Utils (startswith)

startsWith = flip startswith

-- | Convenience function for wrapping strings in quotes
wrapTerms :: String -> String
wrapTerms "" = ""
wrapTerms s | s `startsWith` "Content" = "Content" ++ (wrapTerms $ drop 7 s)
            | s `startsWith` " "       = " " ++ (wrapTerms $ drop 1 s)
            | s `startsWith` "->"      = "->" ++ (wrapTerms $ drop 2 s)
            | s `startsWith` "("       = "(" ++ (wrapTerms $ drop 1 s)
            | s `startsWith` ")"       = ")" ++ (wrapTerms $ drop 1 s)
            | otherwise = let (n, word) = grabTil [' ','-','(',')'] s in
                              (wrapQuotes word) ++ (wrapTerms $ drop n s)
  where
    grabTil :: [Char] -> String -> (Int, String)
    grabTil cs str = let word = takeWhile (\x -> and $ map (x /=) cs) str in
                         (length word, word)

    wrapQuotes :: String -> String
    wrapQuotes str = ('"' : str) ++ "\""

-- | Convenience function for turning a string into the BNFC AST
parseToAst :: String -> Err Expr
parseToAst = pExpr . myLexer . wrapTerms
