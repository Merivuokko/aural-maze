-- This module provides utilities for generating random music based on parameters.

-- |
-- Module      : RandomMusic.Generator
-- Description : Random music generator
-- Copyright   : Copyright (C) 2023-2024 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC
module RandomMusic.Generator (
    generateMusic,
) where

import Control.Monad (replicateM)
import Control.Monad.Reader
import Control.Monad.State.Strict
import System.Random

import RandomMusic.Parameters
import Scherzo.Music.Elementary
import Scherzo.Music.Expr
import Temperament.Equal12

-- | Generate a music expression with given parameters and random number generator
generateMusic :: MusicParams -> StdGen -> MusicExpr
generateMusic params rng = (flip evalState) (GenState rng) . (flip runReaderT) params $ genMusicExpr.runMusicGen

-- | Music generator's state
data GenState = GenState
    { rng :: StdGen
    }

-- | Alias for generation monad
newtype MusicGen a = MusicGen
    { runMusicGen :: ReaderT MusicParams (State GenState) a
    }
    deriving newtype (Functor, Applicative, Monad, MonadReader MusicParams, MonadState GenState)

-- | Execute a random number generation function within MusicGen
withRng :: (StdGen -> (a, StdGen)) -> MusicGen a
withRng f = do
    rng <- gets (.rng)
    let (val, rng') = f rng
    modify' $ \s -> s {rng = rng'}
    pure $! val

genMusicExpr :: MusicGen MusicExpr
genMusicExpr = do
    -- Get number of events
    lr <- asks (.sequential.lengthRange)
    n <- withRng $ uniformR (lr.min, lr.max)
    subexprs <- replicateM n genNoteExpr
    pure $! SequentialExpr $! subexprs

-- | Generate a Note and wrap it in MusicExpr
genNoteExpr :: MusicGen MusicExpr
genNoteExpr = do
    pitch <- genNotePitch
    duration <- genNoteDuration
    pure $!
        NoteExpr $!
            Note
                { pitches = [pitch],
                  duration = duration,
                  articulations = []
                }

-- | Generate a NoteDuration
genNoteDuration :: MusicGen NoteDuration
genNoteDuration = do
    -- This is a temporary solution to be expanded later
    pure
        NoteDuration
            { value = Semibreve,
              dots = 0
            }

-- | Generate a NotePitch
genNotePitch :: MusicGen NotePitch
genNotePitch = do
    pitchRange <- asks (.note.pitchRange)

    -- Convert the NotePitch values to note numbers (assuming enharmonic equivalence)
    let minPitch = notePitchToNumber pitchRange.min
        maxPitch = notePitchToNumber pitchRange.max

    -- Get a random tone
    noteNum <- withRng $! uniformR (minPitch, maxPitch)

    -- Convert the number back to a NotePitch, choosing random enharmonic
    -- representation. In case we hit either the minimum or maximum possible value, or value close to them, the returned value may be enharmonically incorrect.
    useSharp <- withRng uniform
    pure $! numberToNotePitch noteNum useSharp
