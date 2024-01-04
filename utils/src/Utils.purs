module Utils
  ( readFile
  , readLines
  )
  where

import Prelude

import Data.String (Pattern(..), split)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

readFile :: String -> Effect String
readFile filePath = readTextFile UTF8 filePath

readLines :: String -> Effect (Array String)
readLines filePath = do 
  fileContent <- readTextFile UTF8 filePath
  pure (split (Pattern "\n") fileContent)