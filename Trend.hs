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


dot = circle 0.1 # fc black

cross = hrule 0.1 <> vrule 0.1

type UTime = UTCTime

write :: Show a => a -> Di
write c = text(show c) # withEnvelope ( envelope::D R2)
  where l = fromIntegral $ length(show c)
        envelope = rect (l+1) 2
  

push :: Double -> Double -> Di -> Di
push x y = translate $ r2(x,y)

--Avarage difference in time in a list
sumdiff :: [NominalDiffTime] -> NominalDiffTime
sumdiff [] = 0
sumdiff (_:[]) = 0
sumdiff (x1:x2:xs) = (x2-x1)+sumdiff(x2:xs)

meandiff :: [UTCTime] -> NominalDiffTime
meandiff t = sumdiff l / (genericLength l)
  where l = map (\ x -> diffUTCTime x (head t)) t

        
--Our monad with the error handling
--and reader characteristic:

type Env a = ReaderT Choices (ErrorT String Identity) a

runEnv :: Choices -> Env Di -> Either String Di
runEnv choice a  = runIdentity ( runErrorT(runReaderT a choice))

-- We will use Env Di in our functions:

--Lazy:

standartStyles = cycle [ (dot,blue,0.1), (dot,red,0.2)]

-- Order:


order :: [(UTime,Double)] -> [(UTime,Double)]
order l = sortBy (\ (t1,i1) (t2,i2) -> compare t1 t2 ) l

--Transform time into points

timeToDouble :: NominalDiffTime -> Double
timeToDouble a = realToFrac a

timeToPoint:: NominalDiffTime -> [(UTime,Double)] -> [P2]
timeToPoint unit l = map p2 p
  where l' = order l
        (fi,t) = head l'
        p = map (\ (time,i) -> (f time, i)) l'
        f time = timeToDouble $ (diffUTCTime time fi )/unit


-- These gives me curves:
curve :: [P2] -> (Di,C,Double) -> Di
curve l (mark,lcolour,lwidth) = marks <> lines
  where marks = position ( zip l (repeat mark))
        lines = fromVertices l # lc lcolour # lwO lwidth

drawcurves :: [[P2]] -> Env Di
drawcurves l = do m <- ask
                  style <- return $ case Map.lookup "CurveStyle" m of
                    Just(Styles s) -> s ++standartStyles
                    _-> standartStyles
                  return $ mconcat $ zipWith curve l style

--The Axis:

yaxis :: Env Di
yaxis = do m <- ask
           l <- return $ findIntervalle m
           return $ (axis l <> marks l)
                                           
  where findIntervalle m = case Map.lookup "Y-axis" m of
          Just ( Intervalle l) -> l
          _ -> [0.0 , 20.0  .. 100.0]
        axis l = fromVertices $ map p2 [(0,0),(0, last l)]
        ymark n = hrule 1
                <> write n # push (-3) 0
        f n = (p2(0,n),ymark n)
        marks l = position $ map f l


xaxis :: [[UTime]] -> Env Di
xaxis l = do m <- ask

             t <- case Map.lookup "X-axis frequency" m of
               Just(Fre f) -> return f
               _ -> return d
             u <- case Map.lookup "Unitary Time" m of
               Just(Fre u) -> return u
               _ -> return $ meandiff (head l)
             return $ axis u t
               

  where start = minimum $ map head l
        end = maximum $ map last l
        intervalle timefre = takeWhile(<=end)
                             $ iterate (addUTCTime timefre) start

        xmark c = vrule 1 # pad 1.1
                  <> write (utcToDate c) # pad 1.1 # push 0 (-2)

        l' unit timefre = timeToPoint unit
                         $ map (\ i -> (i,0)) (intervalle timefre)

        m timefre = map xmark (intervalle timefre)

        axis u t = position $ zip (l' u t) (m t)


-- The graph:

graph :: [[(UTime,Double)]] -> Env Di
graph t = do m <- ask
             unit <- case Map.lookup "Unitary Time" m of
                          Just (Fre u) -> return u
                          _-> return $ meandiff (onlytime (head t))
             curves <- drawcurves (l unit)
             yax <- yaxis
             xax <- xaxis t'
             return $ mconcat [curves , yax, xax]

  where l unit = map (timeToPoint unit) t
        onlytime list = map (\ (t,_) -> t) list
        t' = map onlytime t

          
