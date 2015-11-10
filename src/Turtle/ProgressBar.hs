{-# LANGUAGE OverloadedStrings #-}

module Turtle.ProgressBar
    ( ProgressBar
    , withProgressBar
    , completeProgressBar
    ) where

import           Control.Monad.STM
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Data.Monoid
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           System.Console.ANSI
import           System.Console.Regions
import           Text.Printf                 (printf)

data ProgressBar = ProgressBar ConsoleRegion (Async ())

completeProgressBar :: ProgressBar -> Maybe (Text, Color) -> IO ()
completeProgressBar (ProgressBar region updater) mmsg = do
    cancel updater
    case mmsg of
        Nothing -> pure ()
        Just (msg, color) -> finishConsoleRegion region (color' <> msg <> reset)
          where
            color' = T.pack (setSGRCode [SetColor Foreground Dull color])

withProgressBar :: Text -> Double -> (ProgressBar -> IO r) -> IO r
withProgressBar label total act =
    withConsoleRegion Linear $ \entire_region ->
    withConsoleRegion (InLine entire_region) $ \label_region ->
    withConsoleRegion (InLine entire_region) $ \bar_region ->
    withConsoleRegion (InLine entire_region) $ \elapsed_region -> do
        elapsedVar <- atomically (newTVar 0)
        let tick = 0.1 -- tick len in seconds

        w <- atomically consoleWidth

        setConsoleRegion label_region $
            let label_len = w `div` 10 in
            T.justifyLeft label_len ' ' (T.take (label_len - 1) label)

        setConsoleRegion bar_region $ do
            elapsed <- readTVar elapsedVar

            let pct         = min 1 (elapsed / total)
                total_bar_w = w `div` 4
                bar_w       = floor (pct * fromIntegral total_bar_w)
                pct_str     = T.pack (printf " % 4d%% " (floor (pct*100) :: Int))
                bar_str     = T.justifyLeft total_bar_w ' ' (T.replicate bar_w "█")

            if pct >= 1
                then pure (T.concat [pct_str, yellow, bar_str, reset, "| "])
                else pure (T.concat [pct_str,         bar_str,        "| "])

        setConsoleRegion elapsed_region $ do
            elapsed <- readTVar elapsedVar
            pure (T.pack (printf "%.1f/%.1f" elapsed total))

        a <- async $ do
                let loop elapsed = do
                        threadDelay (floor (tick * 1000000))
                        let elapsed' = elapsed + tick
                        atomically (writeTVar elapsedVar elapsed')
                        loop elapsed'
                loop 0

        act (ProgressBar entire_region a)

yellow :: Text
yellow = T.pack (setSGRCode [SetColor Foreground Dull Yellow])

reset :: Text
reset = T.pack (setSGRCode [Reset])
