{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

import AuralMaze.Config

musicParams =
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

config =
    Config
        { musicParams = musicParams,
          synthParams = synthParams
        }
