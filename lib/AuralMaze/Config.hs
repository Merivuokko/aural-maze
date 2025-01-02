-- |
-- Module      : AuralMaze.Config
-- Description : Top-level data structure for Aural Maze configuration
-- Copyright   : Copyright (C) 2024 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Stability   : experimental
-- Portability : GHC
--
-- This module defines the data structure for global configuration.
module AuralMaze.Config (
    Config (..),
    module AuralMaze.RandomMusic.Parameters,
    module AuralMaze.Synthesis.Parameters,
    module Scherzo.Music.Elementary,
    module Scherzo.Music.Expr,
) where

import AuralMaze.RandomMusic.Parameters
import AuralMaze.Synthesis.Parameters
import Scherzo.Music.Elementary
import Scherzo.Music.Expr

-- | Global configuration for Aural Maze
data Config = Config
    { -- | Parameters for music generation
      musicParams :: MusicParams,
      -- | Parameters for audio synthesis
      synthParams :: SynthParams
    }
    deriving stock (Eq, Show)
