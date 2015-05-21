module LText.Parser.Types where

import Data.Map as Map
import Data.Conduit.Attoparsec


type Var = String

data Template = Template
  -- ^ It's filename
  FilePath
  -- ^ The occurred positions of each variable declared
  (Map Var [PositionRange])
