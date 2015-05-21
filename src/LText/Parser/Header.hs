module LText.Parser.Header where

-- goal: discover delimeters for expression regex
--   - ignore useless leading characters? (comments)
--   - no spaces _in_ delimiters
--   - `// {$^ foo.js x y %)@}` <- legit

type Var = String

type HeaderSchema = (String, [Var], String)

getHeader :: FilePath -> String -> Either String HeaderSchema
getHeader name string = let string' = words string in
  case findIndex name string' of
    Nothing -> Left "Couldn't find filename `" ++ name ++ "` in `" ++ string ++ "`."
    Just i | i < 1                 -> Left "No left delimiter declared in `" ++ string ++ "`."
           | i == length string'-1 -> Left "No right delimiter declared in `" ++ string ++ "`."
           | otherwise -> Right ( string' !! i-1
                                , init $ drop (i+1) string'
                                , last string'
                                )
