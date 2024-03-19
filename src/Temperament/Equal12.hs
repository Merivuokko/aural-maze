-- |
-- Module      : Temperament.Equal12
-- Description : Implementation of 12-tone equal temperament
-- Copyright   : Copyright (C) 2024 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC
module Temperament.Equal12 (
    notePitchToNumber,
    numberToNotePitch,
    notePitchToFrequency,
) where

import Data.Bool (bool)

import Scherzo.Music.Elementary

-- | Converts a NotePitch to an integer representation. This representation
-- aligns with the MIDI note numbering system, with the distinction that
-- octaves are based on the scientific pitch notation rather than the
-- traditional MIDI octave numbering. In scientific pitch notation, the note
-- C4 corresponds to middle C, which is note number 48.
notePitchToNumber :: NotePitch -> Int
notePitchToNumber note = note.octave * 12 + pitchNum note.name + altNum note.alteration
  where
    altNum :: Alteration -> Int
    altNum = \case
        DoubleFlat -> -2
        Flat -> -1
        Natural -> 0
        Sharp -> 1
        DoubleSharp -> 2

    pitchNum :: PitchName -> Int
    pitchNum = \case
        C -> 0
        D -> 2
        E -> 4
        F -> 5
        G -> 7
        A -> 9
        B -> 11

-- | Converts an integer representing a note back into a NotePitch data structure.
-- The integer should correspond to the scientific pitch notation-based numbering,
-- similar to the MIDI note numbering system. The Bool parameter specifies whether
-- to use sharp (True) or flat (False) representations for notes that can be
-- expressed as either (e.g., C# or Db). Middle C (C4) is represented by the number 48.
numberToNotePitch :: Int -> Bool -> NotePitch
numberToNotePitch num useSharp =
    let octave = num `div` 12
        pitchClass = num `mod` 12
        (name, alteration) = pitchAndAlt pitchClass
    in  NotePitch {..}
  where
    pitchAndAlt :: Int -> (PitchName, Alteration)
    pitchAndAlt = \case
        0 -> (C, Natural)
        1 -> bool (D, Flat) (C, Sharp) useSharp
        2 -> (D, Natural)
        3 -> bool (E, Flat) (D, Sharp) useSharp
        4 -> (E, Natural)
        5 -> (F, Natural)
        6 -> bool (G, Flat) (F, Sharp) useSharp
        7 -> (G, Natural)
        8 -> bool (A, Flat) (G, Sharp) useSharp
        9 -> (A, Natural)
        10 -> bool (B, Flat) (A, Sharp) useSharp
        11 -> (B, Natural)
        _ -> error $! "Impossible: bad note number"

-- | Convert a NotePitch to a frequency.
notePitchToFrequency
    :: forall a
     . (Floating a)
    => a
    -- ^ Base frequency (of A in fourth ocatve)
    -> NotePitch
    -- ^ NotePitch to convert
    -> a
    -- ^ Result frequency
notePitchToFrequency base pitch = base * ((2 ** (1 / 12)) ** noteRelativeToA4)
  where
    noteRelativeToA4 :: a
    noteRelativeToA4 = (fromIntegral $! notePitchToNumber pitch) - 57
