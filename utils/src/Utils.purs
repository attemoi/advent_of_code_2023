module Utils
  ( readLines
  )
  where

import Prelude

import Data.String (Pattern(..), split)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

readLines :: String -> Effect (Array String)
readLines filePath = do 
  fileContent <- readTextFile UTF8 filePath
  pure (split (Pattern "\n") fileContent)