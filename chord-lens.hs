{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Music.Instrument.Chord
import Data.Default
import Control.Lens

data ChordOptions = ChordOptions { 
     _annotationStyle :: ControlAnnotation 
     ,_tuning :: [Note]
     ,_chord :: Chord
     ,_maximumPatternHeight :: Int
    }
    
makeLenses ''ChordOptions

instance Default ChordOptions where
  def = ChordOptions { _annotationStyle = AnnotateMarking, _maximumPatternHeight = 4, _tuning = standardTuning , _chord = (majorChord C) }
  
renderChords (ChordOptions a t c m) = renderGuitarChords a t c m