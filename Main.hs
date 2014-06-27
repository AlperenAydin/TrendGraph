{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

import Settings
import Trend
import Date
import FromFiles

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Shapes
import Diagrams.TwoD.Text
import Diagrams.TrailLike

import qualified Data.Map as Map
import Data.Time.Clock

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader

import System.IO
import System.Environment

points t = map (\ n ->(addUTCTime (n*d) t,timeToDouble  n^2)) [0, 1.0 .. 10]

cross = hrule 1 <> vrule 1

setting = Map.empty

main = do putStrLn " Type the file name :"
          file <- getLine
          timelist <- openFile file ReadMode
          times <- hGetContents timelist
          timenum <- return $ timeListFromString times
          case runEnv setting (graph timenum) of
            (Right s) -> defaultMain( s <> circle 0.1 # fc red :: Diagram B R2)
            (Left s) -> print s
{-timelist <- openFile "TestList.txt" ReadMode
          times <- hGetContents timelist
          timenum <- return $ timeListFromString times-}
{-t <- getCurrentTime
          point <- return $ points t-}
