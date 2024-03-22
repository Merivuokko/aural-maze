# aural-maze

Aura Maze is a command-line application for practising post-tonal solfege skills and training perfect pitch hearing.
It generates random musical pieces which the user can listen to, and then compare their observations with actual notation.

Currently audio is produced by a simple synthesizer called [lambdasound][lambdasound].
Notation is printed in format understood by the [GNU Lilypond][lilypond] music type-seetting tool.

[lambdasound]: https://github.com/Simre1/lambdasound
[lilypond]: https://lilypond.org/

## Status

`aural-maze` is in early stages of development, but it is already useful and all implemented features should work.

## Features

- Produce random music
  - Random pitches with random alterations
  - Multiple sequential notes
- Parameters for configuring
  - Piece length
  - Pitch ranges
  - Output audio tempo
  - Base tuning frequency

## Building

If you don't have a recent Haskell toolchain installed, use [GHCUp][ghcup] to install latest GHC and cabal.
Then use `cabal run` in `aural-maze`'s root directory to build and run the program.

[ghcup]: https://www.haskell.org/ghcup/

## Configuration

`aural-maze` is pretty configurable, but currently you need to edit `src/Main.hs` to adjust its parameters.
In future a dynamic configuration system will be added.
