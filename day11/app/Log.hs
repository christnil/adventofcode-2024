module Log
  ( conditionalPrint,
    verbosePrint,
  )
where

import System.Environment (lookupEnv)
import Control.Monad (when)
import Data.Maybe (fromMaybe)

isEnabled :: String -> IO Bool
isEnabled envVar = do
    envValue <- fromMaybe "false" <$> lookupEnv envVar
    return (envValue == "true")

conditionalPrint :: Show a => String -> a -> IO ()
conditionalPrint envVar message = do
    enabled <- isEnabled envVar
    when enabled $ print message

verbosePrint :: Show a => a -> IO ()
verbosePrint = conditionalPrint "VERBOSE"

