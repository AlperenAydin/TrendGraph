
module FromFiles where

import Settings

import Data.List
import Data.Time.Clock


timeListFromString :: String -> [(UTCTime, Double)]
timeListFromString l = map timeI l'
  where l'= lines l
        timeI l = let (fw, lst) = span (/=',') l
                      in (read (fw) :: UTCTime, read (tail lst) ::Double)

