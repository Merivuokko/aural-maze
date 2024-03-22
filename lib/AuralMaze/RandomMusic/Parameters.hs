-- This module provides data structures for specifying parameters for random music generation.

-- |
-- Module      : AuralMaze.RandomMusic.Parameters
-- Description : Random music generation parameters
-- Copyright   : Copyright (C) 2024 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC
module AuralMaze.RandomMusic.Parameters (
    -- * Weighted values
    Weighted (..),

    -- * Value ranges
    Range (..),

    -- * Music generation parameters
    MusicParams (..),
    SequentialMusicParams (..),
    NoteParams (..),
) where

import Scherzo.Music.Elementary (NotePitch)

-- | Weighted represents a value with a selection weight. These weights are
-- accumulated together to calculate the probability of each choice in random
-- selection.
data Weighted a = Weighted
    { -- | Weight
      weight :: Int,
      value :: a
    }
    deriving stock (Eq, Show)

-- | A range of values that may be generated
data Range a
    = Range
    { -- | Minimum value (inclusive)
      min :: a,
      -- | Maximum value
      max :: a
    }
    deriving stock (Eq, Show)

-- | Parameters for creating complete music expressions
data MusicParams = MusicParams
    { -- | Sequential music parameters
      sequential :: SequentialMusicParams,
      note :: NoteParams
    }
    deriving stock (Eq, Show)

-- | Parameters for sequential music expression generation
data SequentialMusicParams = SequentialMusicParams
    { -- | A range for the length of the generated music (in events, not music duration)
      lengthRange :: Range Int
    }
    deriving stock (Eq, Show)

-- | Parameters for single note generation
data NoteParams = NoteParams
    { pitchRange :: Range NotePitch
    }
    deriving stock (Eq, Show)
