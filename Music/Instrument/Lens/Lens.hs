{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Music.Instrument.Chord
import Data.Default
import Control.Lens

data GuitarChordOptions = GuitarChordOptions { 
      _annotationStyle :: ControlAnnotation 
     ,_firstTuningFirst :: Bool
     ,_vertical :: Bool
     ,_tuning :: [Note]
     ,_chord :: Chord
     ,_maximumPatternHeight :: Int
    }
    
makeLenses ''GuitarChordOptions

instance Default GuitarChordOptions where
  def = GuitarChordOptions {
            _annotationStyle = AnnotateMarking
          , _firstTuningFirst = False
          , _vertical = True
          , _maximumPatternHeight = 4
          , _tuning = standardTuning
          , _chord = majorChord C
        }

instance Show GuitarChordOptions where
  show (GuitarChordOptions a f v t c m) = renderGuitarChords a f v t c m
