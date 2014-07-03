{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

import Settings
import Trend
import Date
import FromFiles

import Diagrams.Prelude hiding ((<>))
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Backend.CmdLine
import Diagrams.TwoD.Shapes
import Diagrams.TwoD.Text
import Diagrams.TrailLike
import Options.Applicative

import qualified Data.Map as Map
import Data.List
import Data.Time.Clock

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader

import System.IO
import System.Environment


-- Source file flag:

data Source = Source String

instance Parseable Source where
  parser = Source
           <$> strOption ( long "Source"
                           <> metavar "FILE"
                           <> help "File that has the info")

points t = map (\ n ->(addUTCTime (n*d) t,timeToDouble  n^2)) [0, 1.0 .. 10]

cross = hrule 1 <> vrule 1

setting = Map.empty

graphfromFile src = do file <- openFile src ReadMode
                       times <- hGetContents file
                       timenum <- return $ timeListFromString times
                       case runEnv setting ( graph [timenum]) of
                         (Right s) -> return $ (s <> circle 0.1 # fc red)
                         (Left st) -> return $ (circle 1 # fc red)
                         
main = mainWith ( \ (Source src) -> graphfromFile src)


  
{-timelist <- openFile "TestList.txt" ReadMode
          times <- hGetContents timelist
          timenum <- return $ timeListFromString times-}
{-t <- getCurrentTime
          point <- return $ points t-}
