-- |
-- Module      : Synthesis.Instruments
-- Description : Synthetic instrument definitions
-- Copyright   : Copyright (C) 2024 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC
--
-- This module defines synthetic instruments.
module Synthesis.Instruments (
    instrumentOne,
) where

import LambdaSound

-- | Instrument is a function from duration and frequency to sound.
instrumentOne :: Duration -> Hz -> Sound T Pulse
instrumentOne duration freq = sound
  where
    sound :: Sound T Pulse
    sound = applyEnvelope envelope $! setDuration duration $! triangleWave freq

    envelope :: Envelope
    envelope =
        let attack = min 0.01 (duration / 10)
            decay = min 1 (duration * 0.4)
            release = min 0.2 (duration - attack - decay)
        in  Envelope
                { attack = attack,
                  decay = decay,
                  sustain = 0.8,
                  release = release
                }
