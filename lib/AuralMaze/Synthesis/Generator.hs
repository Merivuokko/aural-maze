-- |
-- Module      : AuralMaze.Synthesis.Generator
-- Description : Conversion from music expressions to audio signal
-- Copyright   : Copyright (C) 2024 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC
module AuralMaze.Synthesis.Generator (
    generateSignal,
) where

import Control.Monad.Reader

import LambdaSound

import AuralMaze.Synthesis.Instruments
import AuralMaze.Synthesis.Parameters
import AuralMaze.Temperament.Equal12
import Scherzo.Music.Elementary
import Scherzo.Music.Expr

-- | Generate audio signal froma music expression
generateSignal
    :: SynthParams
    -- ^ Synthesis parameters
    -> MusicExpr
    -- ^ Input music
    -> Sound T Pulse
    -- ^ Output audio signal
generateSignal params music = runReader (genMusic music).runAudioGen params

-- | Alias for generation monad
newtype AudioGen a = AudioGen
    { runAudioGen :: Reader SynthParams a
    }
    deriving newtype (Functor, Applicative, Monad, MonadReader SynthParams)

genMusic :: MusicExpr -> AudioGen (Sound T Pulse)
genMusic music = do
    tempo <- asks (.tempoCrotchetsPerMinute)
    let defaultTempo = 4 * 60
    -- Amount of chortchets per minute, when crochet = 0.25
    sound <- genMusicExpr music
    pure $! scaleDuration (defaultTempo / tempo) sound

genMusicExpr :: MusicExpr -> AudioGen (Sound T Pulse)
genMusicExpr = \case
    SequentialExpr xs -> traverse genMusic xs >>= (pure $!) . sequentially
    SimultaneousExpr xs -> traverse genMusic xs >>= (pure $!) . parallel
    NoteExpr note -> genNote note
    RestExpr rest -> genRest rest
    BarExpr -> pure $! setDuration 0 silence

-- | Generate audio for a note
genNote :: Note -> AudioGen (Sound T Pulse)
genNote note = do
    a4Tuning <- asks $ realToFrac . (.baseTuning)
    let len = fromRational . toRational . durationLength $ note.duration
        pitches = fmap (notePitchToFrequency a4Tuning) note.pitches
    pure $! parallel $! fmap (instrumentOne len) pitches

genRest :: Rest -> AudioGen (Sound T Pulse)
genRest rest = pure $! setDuration (fromRational . toRational . durationLength $ rest.duration) silence
