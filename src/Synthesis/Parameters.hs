-- |
-- Module      : Synthesis.Parameters
-- Description : Audio synthesis parameters
-- Copyright   : Copyright (C) 2024 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC
module Synthesis.Parameters (
    SynthParams (..),
) where

-- | Parameters for controlling audio synthesis
data SynthParams = SynthParams
    { -- | Tempo of generated output in quarter notes per minute
      tempoCrotchetsPerMinute :: Float,
      -- | Base tuning frequency: frequency of one-line A
      baseTuning :: Float
    }
    deriving stock (Eq, Show)
