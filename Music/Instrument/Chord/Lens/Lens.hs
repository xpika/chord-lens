{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Music.Instrument.Chord.Lens.Lens (
 annotationStyle
,firstTuningFirst
,vertical
,tuning
,concept
,maximumPatternHeight
,startingFrom
)
 where

import Music.Instrument.Chord hiding (scale)
import Data.Default
import Data.Functor


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
      _allowOpens :: Bool
     ,_annotationStyle :: ControlAnnotation 
     ,_annotateFrets :: Bool
     ,_firstTuningFirst :: Bool
     ,_vertical :: Bool
     ,_tuning :: [Note]
     ,_concept' :: GuitarChordType
     ,_maximumPatternHeight :: Int
     ,_startingFrom :: Int
    }
    
{-
makeLenses ''GuitarChordOptions

ghc -ddump-splices 
-}

--allowOpens :: Lens' GuitarChordOptions Bool
allowOpens
  _f_a2Ev
  (GuitarChordOptions __allowOpens'_a2Ew
                      __annotationStyle_a2Ey
                      __annotateFrets_a2Ez
                      __firstTuningFirst_a2EA
                      __vertical_a2EB
                      __tuning_a2EC
                      __concept'_a2ED
                      __maximumPatternHeight_a2EE
                      __startingFrom_a2EF)
  = ((\ __allowOpens_a2Ex
        -> GuitarChordOptions
             __allowOpens_a2Ex
             __annotationStyle_a2Ey
             __annotateFrets_a2Ez
             __firstTuningFirst_a2EA
             __vertical_a2EB
             __tuning_a2EC
             __concept'_a2ED
             __maximumPatternHeight_a2EE
             __startingFrom_a2EF)
     Data.Functor.<$> (_f_a2Ev __allowOpens'_a2Ew))

{-# INLINE allowOpens #-}
--annotateFrets :: Lens' GuitarChordOptions Bool
annotateFrets
  _f_a2EG
  (GuitarChordOptions __allowOpens_a2EH
                      __annotationStyle_a2EI
                      __annotateFrets'_a2EJ
                      __firstTuningFirst_a2EL
                      __vertical_a2EM
                      __tuning_a2EN
                      __concept'_a2EO
                      __maximumPatternHeight_a2EP
                      __startingFrom_a2EQ)
  = ((\ __annotateFrets_a2EK
        -> GuitarChordOptions
             __allowOpens_a2EH
             __annotationStyle_a2EI
             __annotateFrets_a2EK
             __firstTuningFirst_a2EL
             __vertical_a2EM
             __tuning_a2EN
             __concept'_a2EO
             __maximumPatternHeight_a2EP
             __startingFrom_a2EQ)
     Data.Functor.<$> (_f_a2EG __annotateFrets'_a2EJ))
{-# INLINE annotateFrets #-}
--annotationStyle :: Lens' GuitarChordOptions ControlAnnotation
annotationStyle
  _f_a2ER
  (GuitarChordOptions __allowOpens_a2ES
                      __annotationStyle'_a2ET
                      __annotateFrets_a2EV
                      __firstTuningFirst_a2EW
                      __vertical_a2EX
                      __tuning_a2EY
                      __concept'_a2EZ
                      __maximumPatternHeight_a2F0
                      __startingFrom_a2F1)
  = ((\ __annotationStyle_a2EU
        -> GuitarChordOptions
             __allowOpens_a2ES
             __annotationStyle_a2EU
             __annotateFrets_a2EV
             __firstTuningFirst_a2EW
             __vertical_a2EX
             __tuning_a2EY
             __concept'_a2EZ
             __maximumPatternHeight_a2F0
             __startingFrom_a2F1)
     Data.Functor.<$> (_f_a2ER __annotationStyle'_a2ET))
{-# INLINE annotationStyle #-}
--concept' :: Lens' GuitarChordOptions GuitarChordType
concept'
  _f_a2F2
  (GuitarChordOptions __allowOpens_a2F3
                      __annotationStyle_a2F4
                      __annotateFrets_a2F5
                      __firstTuningFirst_a2F6
                      __vertical_a2F7
                      __tuning_a2F8
                      __concept''_a2F9
                      __maximumPatternHeight_a2Fb
                      __startingFrom_a2Fc)
  = ((\ __concept'_a2Fa
        -> GuitarChordOptions
             __allowOpens_a2F3
             __annotationStyle_a2F4
             __annotateFrets_a2F5
             __firstTuningFirst_a2F6
             __vertical_a2F7
             __tuning_a2F8
             __concept'_a2Fa
             __maximumPatternHeight_a2Fb
             __startingFrom_a2Fc)
     Data.Functor.<$> (_f_a2F2 __concept''_a2F9))
{-# INLINE concept' #-}
--firstTuningFirst :: Lens' GuitarChordOptions Bool
firstTuningFirst
  _f_a2Fd
  (GuitarChordOptions __allowOpens_a2Fe
                      __annotationStyle_a2Ff
                      __annotateFrets_a2Fg
                      __firstTuningFirst'_a2Fh
                      __vertical_a2Fj
                      __tuning_a2Fk
                      __concept'_a2Fl
                      __maximumPatternHeight_a2Fm
                      __startingFrom_a2Fn)
  = ((\ __firstTuningFirst_a2Fi
        -> GuitarChordOptions
             __allowOpens_a2Fe
             __annotationStyle_a2Ff
             __annotateFrets_a2Fg
             __firstTuningFirst_a2Fi
             __vertical_a2Fj
             __tuning_a2Fk
             __concept'_a2Fl
             __maximumPatternHeight_a2Fm
             __startingFrom_a2Fn)
     Data.Functor.<$> (_f_a2Fd __firstTuningFirst'_a2Fh))
{-# INLINE firstTuningFirst #-}
--maximumPatternHeight :: Lens' GuitarChordOptions Int
maximumPatternHeight
  _f_a2Fo
  (GuitarChordOptions __allowOpens_a2Fp
                      __annotationStyle_a2Fq
                      __annotateFrets_a2Fr
                      __firstTuningFirst_a2Fs
                      __vertical_a2Ft
                      __tuning_a2Fu
                      __concept'_a2Fv
                      __maximumPatternHeight'_a2Fw
                      __startingFrom_a2Fy)
  = ((\ __maximumPatternHeight_a2Fx
        -> GuitarChordOptions
             __allowOpens_a2Fp
             __annotationStyle_a2Fq
             __annotateFrets_a2Fr
             __firstTuningFirst_a2Fs
             __vertical_a2Ft
             __tuning_a2Fu
             __concept'_a2Fv
             __maximumPatternHeight_a2Fx
             __startingFrom_a2Fy)
     Data.Functor.<$> (_f_a2Fo __maximumPatternHeight'_a2Fw))
{-# INLINE maximumPatternHeight #-}
--startingFrom :: Lens' GuitarChordOptions Int
startingFrom
  _f_a2Fz
  (GuitarChordOptions __allowOpens_a2FA
                      __annotationStyle_a2FB
                      __annotateFrets_a2FC
                      __firstTuningFirst_a2FD
                      __vertical_a2FE
                      __tuning_a2FF
                      __concept'_a2FG
                      __maximumPatternHeight_a2FH
                      __startingFrom'_a2FI)
  = ((\ __startingFrom_a2FJ
        -> GuitarChordOptions
             __allowOpens_a2FA
             __annotationStyle_a2FB
             __annotateFrets_a2FC
             __firstTuningFirst_a2FD
             __vertical_a2FE
             __tuning_a2FF
             __concept'_a2FG
             __maximumPatternHeight_a2FH
             __startingFrom_a2FJ)
     Data.Functor.<$> (_f_a2Fz __startingFrom'_a2FI))
{-# INLINE startingFrom #-}
--tuning :: Lens' GuitarChordOptions [Note]
tuning
  _f_a2FK
  (GuitarChordOptions __allowOpens_a2FL
                      __annotationStyle_a2FM
                      __annotateFrets_a2FN
                      __firstTuningFirst_a2FO
                      __vertical_a2FP
                      __tuning'_a2FQ
                      __concept'_a2FS
                      __maximumPatternHeight_a2FT
                      __startingFrom_a2FU)
  = ((\ __tuning_a2FR
        -> GuitarChordOptions
             __allowOpens_a2FL
             __annotationStyle_a2FM
             __annotateFrets_a2FN
             __firstTuningFirst_a2FO
             __vertical_a2FP
             __tuning_a2FR
             __concept'_a2FS
             __maximumPatternHeight_a2FT
             __startingFrom_a2FU)
     Data.Functor.<$> (_f_a2FK __tuning'_a2FQ))
{-# INLINE tuning #-}
--vertical :: Lens' GuitarChordOptions Bool
vertical
  _f_a2FV
  (GuitarChordOptions __allowOpens_a2FW
                      __annotationStyle_a2FX
                      __annotateFrets_a2FY
                      __firstTuningFirst_a2FZ
                      __vertical'_a2G0
                      __tuning_a2G2
                      __concept'_a2G3
                      __maximumPatternHeight_a2G4
                      __startingFrom_a2G5)
  = ((\ __vertical_a2G1
        -> GuitarChordOptions
             __allowOpens_a2FW
             __annotationStyle_a2FX
             __annotateFrets_a2FY
             __firstTuningFirst_a2FZ
             __vertical_a2G1
             __tuning_a2G2
             __concept'_a2G3
             __maximumPatternHeight_a2G4
             __startingFrom_a2G5)
     Data.Functor.<$> (_f_a2FV __vertical'_a2G0))
{-# INLINE vertical #-}


instance Default GuitarChordOptions where
  def = GuitarChordOptions {
            _allowOpens = False
          , _annotationStyle = AnnotateMarking
          , _annotateFrets = False
          , _firstTuningFirst = False
          , _vertical = True
          , _tuning = standardTuning
          , _concept' = GuitarChordTypeChord (majorChord C)
          , _maximumPatternHeight = 4
          , _startingFrom = 0
        }

lens sa sbt afb s = sbt s <$> afb (sa s)

concept :: (Functor f, GuitarChordTypeClass a) => (a -> f a) -> GuitarChordOptions -> f GuitarChordOptions
concept = lens ( \x -> fromGuitarChordType (_concept' x)) (\a b -> a { _concept' = (toGuitarChordType b) })

chord :: Functor f => (Chord -> f Chord) -> GuitarChordOptions -> f GuitarChordOptions
chord = concept

scale :: Functor f => (Scale-> f Scale) -> GuitarChordOptions -> f GuitarChordOptions
scale = concept

instance Show GuitarChordOptions where
  show (GuitarChordOptions o a af f v t (GuitarChordTypeChord c) m fr) = renderGuitarChord o a af f v t c m fr
  show (GuitarChordOptions o a af f v t (GuitarChordTypeScale s) m fr) = renderGuitarChord o a af f v t s m fr
