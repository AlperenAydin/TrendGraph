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
import Diagrams.Envelope

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

write :: Show a => a -> Di
write c = text(show c) # withEnvelope ( envelope::D R2)
  where l = fromIntegral $ length(show c)
        envelope = rect (l+1) 2
  

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

xaxis :: [(UTime,Double)] -> Env Di
xaxis t = do m <- ask
             case Map.lookup "XAxisFrequency" m of
               Just(Frequency f) -> return $ axis f
               _-> return $ circle 01 # fc red <> axis 7
  where lnth = length t
        l = timeToPoint $ map projX t
        projX (i,_) = (i,0)
        t' n = indmod n t
        l' n = indmod n l
        xmark c = vrule 1 # pad 1.1
                  <> write (utcToDate c) # pad 1.1 # push 0 (-2)
                  <> circle 0.1 # fc blue
        m n = map (\ (t,_) -> xmark t) (t' n)
        axis n = position (zip (l' n) (m n))

indmod :: Int -> [a] -> [a]
indmod n list = foldl f [] [0 .. (length list)-1]
  where f xs x = if x `mod` n == 0
                    then xs++[list!!x]
                         else xs

                   
--Combining the previous functions
graph :: [(UTime,Double)] -> Env Di
graph t = do marks <- markers l
             curve <- drawcurve l
             yax <- yaxis
             xax <- xaxis t
             return $ mconcat [marks, curve, yax,xax]
 where l = timeToPoint t
