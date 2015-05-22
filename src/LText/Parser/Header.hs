module LText.Parser.Header where

import Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit.Lift as CLift
import qualified Data.Text as T
import Control.Monad.Morph
import Control.Monad.Trans.Except
import Control.Monad.Trans.Error


-- goal: discover delimeters for expression regex
--   - ignore useless leading characters? (comments)
--   - no spaces _in_ delimiters
--   - `//{$^ x y %)@}` <- legit



type Var = String

type HeaderSchema = (String, [Var], String)

getHeader :: FilePath -> String -> Except String HeaderSchema
getHeader name string = let string' = words string in
  case string' of
    []                 -> throwE $ "No header declared in `" ++ name ++"`."
    xs | length xs < 2 -> throwE $ "No delimiters declared in the header of `" ++ name ++"`."
       | otherwise -> return ( head string'
                             , init $ drop 1 string'
                             , last string'
                             )

header :: Monad m => FilePath -> Conduit T.Text m HeaderSchema
header name =
  await >>= maybe (return ()) go
  where
    go input = case runExcept $ getHeader name $ T.unpack input of
      Left s -> error s
      Right h -> yield h >> header name
