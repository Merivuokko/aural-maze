-- |
-- Module      : Main
-- Description : Entry point to Aural Maze
-- Copyright   : Copyright (C) 2024 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Portability : GHC
module Main (main) where

import Data.Text.IO qualified as T
import System.Random (initStdGen)

import LambdaSound

import RandomMusic.Generator
import RandomMusic.Parameters
import Scherzo.Format.LilyPond.Writer
import Scherzo.Music.Elementary
import Synthesis.Generator
import Synthesis.Parameters

main :: IO ()
main = do
    let musicParams =
            MusicParams
                { sequential =
                    SequentialMusicParams
                        { lengthRange = Range 3 3
                        },
                  note =
                    NoteParams
                        { pitchRange =
                            Range
                                { min = NotePitch {name = C, octave = 2, alteration = Natural},
                                  max = NotePitch {name = C, octave = 6, alteration = Natural}
                                }
                        }
                }
        synthParams =
            SynthParams
                { tempoCrotchetsPerMinute = 180,
                  baseTuning = 442.0
                }
    rng <- initStdGen
    let music = generateMusic musicParams rng
        sound = generateSignal synthParams music
    T.putStrLn $! musicToLilyPond music
    play 48000 1.0 sound
