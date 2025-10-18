module MyLib 
  ( someFunc
  -- Re-export HydraPay modules for easy access
  , module HydraPay.Client
  , module HydraPay.API.Types
  ) where

import HydraPay.Client
import HydraPay.API.Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"
