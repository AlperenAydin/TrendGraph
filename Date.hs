{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}


module Date where

import Data.Time
import Data.Time.Clock
import Data.List

months = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"]


data Date = Date (Int, String, Int)

instance Show Date where
  show (Date(d,m,y)) = show d ++ " " ++ m 

utcToDate :: UTCTime -> Date
utcToDate t = let s  = take 10 (show t)
                  s' = words $ map (\ c -> if c=='-' then ' ' else c) s
                  [y,m,d]  = map ( \ t -> read t :: Int) s'
              in Date(d, months !! m, y) 

m=60 :: NominalDiffTime
h=60*m
d=24*h
