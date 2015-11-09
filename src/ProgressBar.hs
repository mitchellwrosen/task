{-# LANGUAGE OverloadedStrings #-}

module ProgressBar
    ( ProgressBar
    , withProgressBar
    , completeProgressBar
    , crashProgressBar
    ) where

import           Control.Monad
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

completeProgressBar :: ProgressBar -> Text -> IO ()
completeProgressBar (ProgressBar region updater) msg = do
    cancel updater
    finishConsoleRegion region (green <> msg <> reset)

crashProgressBar :: ProgressBar -> Text -> IO ()
crashProgressBar (ProgressBar region updater) msg = do
    cancel updater
    finishConsoleRegion region (red <> msg <> reset)

withProgressBar :: Text -> Double -> (ProgressBar -> IO r) -> IO r
withProgressBar label total act =
    withConsoleRegion Linear $ \r -> do
        pctVar <- atomically (newTVar 0)

        w <- atomically consoleWidth
        setConsoleRegion r $ do
            pct <- min 1 <$> readTVar pctVar

            let label_len   = w `div` 10
                total_bar_w = w `div` 4
                bar_w       = floor (pct * fromIntegral total_bar_w)
                pct_str     = T.pack (printf " % 4d%% " (floor (pct*100) :: Int))
                bar_str     = T.justifyLeft total_bar_w ' ' (T.replicate bar_w "█")
                label'      = T.justifyLeft label_len ' ' (T.take (label_len - 1) label)

            if pct >= 1
                then pure (T.concat [yellow, label', pct_str, bar_str, "|", reset])
                else pure (T.concat [        label', pct_str, bar_str, "|"       ])

        a <- async $ do
                let tick = 0.1 -- tick len in seconds
                    loop elapsed = do
                        threadDelay (floor (tick * 1000000))
                        let elapsed' = elapsed + tick
                            pct = elapsed' / total

                        -- Write ONE pct >= 1, then stop.
                        atomically (writeTVar pctVar pct)
                        when (pct < 1) $
                            loop elapsed'
                loop 0

        act (ProgressBar r a)

green :: Text
green = T.pack (setSGRCode [SetColor Foreground Dull Green])

yellow :: Text
yellow = T.pack (setSGRCode [SetColor Foreground Dull Yellow])

red :: Text
red = T.pack (setSGRCode [SetColor Foreground Dull Red])

reset :: Text
reset = T.pack (setSGRCode [Reset])
