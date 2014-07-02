{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

module Settings where

import GHC.Read

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.TwoD.Shapes
import Diagrams.TwoD.Text
import Diagrams.TrailLike

import qualified Data.Map as Map
import Data.Time.Clock

--Convenient Type Versions:

type C = Colour Double

type Di= Diagram B R2

type Setting = String

--Definitions of the settings

data Value = Styles [(Di,C,Double)]
                 | Intervalle [Double]
                 | Fre NominalDiffTime

type Choices = Map.Map Setting Value
