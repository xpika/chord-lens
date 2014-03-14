{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module  Music.Instrument.Lens.Lens where

import Music.Instrument.Chord hiding (scale)
import Data.Default
import Control.Lens hiding (scale)


data GuitarChordType = GuitarChordTypeChord Chord
                     | GuitarChordTypeScale Scale
  deriving Show

class PositionPatternProgression a => GuitarChordTypeClass a where
      toGuitarChordType :: a -> GuitarChordType
      fromGuitarChordType :: GuitarChordType -> a

instance GuitarChordTypeClass Chord where
  toGuitarChordType c = GuitarChordTypeChord c
  fromGuitarChordType (GuitarChordTypeChord c) = c


instance GuitarChordTypeClass Scale where
  toGuitarChordType c = GuitarChordTypeScale  c
  fromGuitarChordType (GuitarChordTypeScale c) = c

data GuitarChordOptions = GuitarChordOptions { 
      _annotationStyle :: ControlAnnotation 
     ,_firstTuningFirst :: Bool
     ,_vertical :: Bool
     ,_tuning :: [Note]
     ,_concept' :: GuitarChordType
     ,_maximumPatternHeight :: Int
     ,_from :: Int
    }
    
makeLenses ''GuitarChordOptions

instance Default GuitarChordOptions where
  def = GuitarChordOptions {
            _annotationStyle = AnnotateMarking
          , _firstTuningFirst = False
          , _vertical = True
          , _tuning = standardTuning
          , _concept' = GuitarChordTypeChord (majorChord C)
          , _maximumPatternHeight = 4
          , _from = 0
        }

concept :: (Functor f, GuitarChordTypeClass a) => (a -> f a) -> GuitarChordOptions -> f GuitarChordOptions
concept = lens ( \x -> fromGuitarChordType (view concept' x)) (\a b -> set concept' (toGuitarChordType b) $ a )

chord :: Functor f => (Chord -> f Chord) -> GuitarChordOptions -> f GuitarChordOptions
chord = concept

scale :: Functor f => (Scale-> f Scale) -> GuitarChordOptions -> f GuitarChordOptions
scale = concept

instance Show GuitarChordOptions where
  show (GuitarChordOptions a f v t (GuitarChordTypeChord c) m fr) = renderGuitarChord a f v t c m fr
  show (GuitarChordOptions a f v t (GuitarChordTypeScale s) m fr) = renderGuitarChord a f v t s m fr
