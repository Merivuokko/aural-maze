-- |
-- Module      : Main
-- Description : Entry point to Aural Maze
-- Copyright   : Copyright (C) 2024 Aura Kelloniemi
-- License     : GPL-3
-- Maintainer  : kaura.dev@sange.fi
-- Portability : GHC
module Main (main) where

import Data.Text qualified as T
import Data.Text.IO qualified as T
import System.Exit

import LambdaSound
import Language.Haskell.Interpreter
import System.Random (initStdGen)

import AuralMaze.Config
import AuralMaze.RandomMusic.Generator
import AuralMaze.Synthesis.Generator
import Scherzo.Format.LilyPond.Writer

main :: IO ()
main = do
    configResult <- runInterpreter $ do
        loadModules ["config.hs"]
        modules <- getLoadedModules
        setTopLevelModules modules
        interpret "config" (as :: Config)
    config <- case configResult of
        Right config -> pure $! config
        Left err -> do
            T.putStrLn "Failed to load configuration:"
            T.putStrLn . hintErrorText $ err
            exitWith $! ExitFailure 1
    rng <- initStdGen
    let music = generateMusic config.musicParams rng
        sound = generateSignal config.synthParams music
    T.putStrLn $! musicToLilyPond music
    play 48000 1.0 sound

hintErrorText :: InterpreterError -> T.Text
hintErrorText =
    T.pack . \case
        UnknownError err -> err
        WontCompile ghcErr -> unlines $! fmap (.errMsg) ghcErr
        NotAllowed err -> err
        GhcException err -> err
