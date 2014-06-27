{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

module Trend where

import Settings
import Date

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Shapes
import Diagrams.TwoD.Text
import Diagrams.TrailLike

import qualified Data.Map as Map
import Data.Time.Clock
import Data.List

import Control.Monad.Identity
import Control.Monad.Error
import Control.Monad.Reader

--Convenience:

type C=Colour Double

dot = circle 0.1 # fc black

type UTime = UTCTime

write = text.show

push :: Double -> Double -> Di -> Di
push x y = translate $ r2(x,y)

--Our monad with the error handling
--and reader characteristic:

type Env a = ReaderT Choices (ErrorT String Identity) a

runEnv :: Choices -> Env Di -> Either String Di
runEnv choice a  = runIdentity ( runErrorT(runReaderT a choice))

-- We will use Env Di in our functions:

-- These take a list of points
markers :: [P2] -> Env Di
markers l = do m <- ask
               case Map.lookup "markers" m of
                 Nothing -> return $ position (zip l (repeat dot))
                 Just (Shape s) -> return $ position (zip l (repeat s))
                 _ -> throwError " Markers input not a shape"


drawcurve :: [P2] -> Env Di
drawcurve l = do m <- ask
                 curve1 <- case Map.lookup "Curve Width" m of
                   Just( Wdth w) -> return $ curve0 # lwO w
                   _-> return $ curve0 
                 case Map.lookup "Curve Colour" m of
                   Nothing -> return $ curve1 # lc blue
                   Just (Col c) -> return $ curve1 # lc c
                   _ -> throwError "Curve Colour not a colour"
  where curve0 = fromVertices l

-- Order:


order :: [(UTime,Double)] -> [(UTime,Double)]
order l = sortBy (\ (t1,i1) (t2,i2) -> compare t1 t2 ) l

--Avarage difference in time in a list
sumdiff :: [NominalDiffTime] -> NominalDiffTime
sumdiff [] = 0
sumdiff (_:[]) = 0
sumdiff (x1:x2:xs) = (x2-x1)+sumdiff(x2:xs)

meandiff :: [UTCTime] -> NominalDiffTime
meandiff t = sumdiff l / (genericLength l)
  where l = map (\ x -> diffUTCTime x (head t)) t

--Transform time into points

timeToDouble :: NominalDiffTime -> Double
timeToDouble a = realToFrac a

timeToPoint:: [(UTime,Double)] -> [P2]
timeToPoint l = map p2 p
  where l' = order l
        (fi,t) = head l'
        p = map (\ (time,i) -> (f time, i)) l'
        mean = meandiff $ map (\(time,i) -> time) l'
        f time = timeToDouble $ (diffUTCTime time fi )/mean

--
yaxis :: Env Di
yaxis = do m <- ask
           case Map.lookup "Y-axis" m of
             Just (Intervalle l) -> return $ yaxis l <> f' l
             _ -> return $ yaxis ls <> f' ls
       where yaxis l = fromVertices $ map p2 [(0,0),(0, last l)]
             ymark n = hrule 1
                       <> write n # push (-3) 0 # pad 1.6
             f n = (p2(0,n),ymark n)
             f' list = position $ map f list
             ls = [0.0 , 20.0 .. 100.0]


--Combining the previous functions
graph :: [(UTime,Double)] -> Env Di
graph t = do marks <- markers l
             curve <- drawcurve l
             yax <- yaxis
             return $ mconcat [marks, curve, yax]
 where l = timeToPoint t
