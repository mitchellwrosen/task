{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Turtle.Task
    ( task
    , taskStrict
    ) where

import Turtle.ProgressBar

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.Managed
import qualified Data.ByteString              as BS
import           Data.Hashable
import           Data.IORef
import           Data.Maybe
import           Data.Serialize               (encode, decode)
import qualified Data.Text                    as T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Prelude                      hiding (FilePath, log)
import           System.Console.ANSI
import           System.Exit
import           System.IO                    (hClose)
import qualified System.Process               as Process
import           System.Random
import           Turtle

task :: Text -> Shell Text -> Maybe Text -> IO Bool
task cmd stdin_shell title = fmap isJust $
    task_ (\sout log -> sh (sout >>= liftIO . log))
          cmd
          stdin_shell
          title

taskStrict :: Text -> Shell Text -> Maybe Text -> IO (Maybe Text)
taskStrict cmd stdin_shell title = task_ (\sout log -> strict (do
                                 txt <- sout
                                 liftIO (log txt)
                                 pure txt))
                             cmd
                             stdin_shell
                             title

task_ :: forall a.
         (Shell Text -> (Text -> IO ()) -> IO a) -- actin to perform on stdout of process
      -> Text                                    -- command
      -> Shell Text                              -- stdin
      -> Maybe Text                              -- title (defaults to command)
      -> IO (Maybe a)
task_ onStdout cmd stdin_shell mtitle = do
    stdin_lines <- fold stdin_shell (Fold (\f x -> \xs -> f (x:xs)) id (\f -> f []))

    let h     = hash (cmd, stdin_lines)
        title = fromMaybe cmd mtitle

    mktree (".tasks" </> hashToFilePath h </> "logs")

    prev_times <- readPrevTimes h
    estimate <-
        case prev_times of
            [] -> randomRIO (10, 30)
            _  -> pure (sum prev_times / fromIntegral (length prev_times))

    hInLock  <- newEmptyMVar
    let tryClose :: Handle -> IO ()
        tryClose hdl = do
            first <- tryPutMVar hInLock ()
            when first (hClose hdl)

        open :: IO (Handle, Handle, Handle, Process.ProcessHandle)
        open = do
            (Just hIn, Just hOut, Just hErr, ph) <- Process.createProcess p
            pure (hIn, hOut, hErr, ph)
          where
            p :: Process.CreateProcess
            p = (Process.shell (T.unpack cmd))
                    { Process.std_in  = Process.CreatePipe
                    , Process.std_out = Process.CreatePipe
                    , Process.std_err = Process.CreatePipe
                    }

        close :: (Handle, Handle, Handle, Process.ProcessHandle) -> IO ()
        close (hIn, _, _, ph) = do
            tryClose hIn
            Process.terminateProcess ph

    -- Keep track of whether or not we ever wrote to stderr, so we know if we
    -- should color the output line green (no output) or yellow (output).
    wrote_to_stderr_ref <- newIORef False

    now_fp <- fromText . T.pack . formatTime defaultTimeLocale "%F_%X%Q" <$> getCurrentTime
    let log_fp = ".tasks" </> hashToFilePath h </> "logs" </> now_fp
        log_fp_txt = T.pack (fpToString log_fp)

        action :: IO (a, ExitCode)
        action =
            withLogfile log_fp $ \(log_handle, logStdout, logStderr) -> do
                outhandle log_handle $
                        pure ("Command: " <> cmd)
                    <|> "Stdin lines:"
                    <|> select stdin_lines
                    <|> "--------------------"

                bracket open close $ \(hIn, hOut, hErr, ph) -> do
                    let feedIn :: IO ()
                        feedIn = do
                            outhandle hIn (select stdin_lines)
                            tryClose hIn

                        feedOut :: IO a
                        feedOut = onStdout (inhandle hOut) logStdout

                        feedErr :: IO ()
                        feedErr = sh $ do
                            txt <- inhandle hErr
                            liftIO $ do
                                writeIORef wrote_to_stderr_ref True
                                logStderr txt

                    started <- getPOSIXTime

                    ((_, stdoutTxt), _) <- feedIn `concurrently` feedOut `concurrently` feedErr

                    code <- Process.waitForProcess ph
                    when (code == ExitSuccess) $ do
                        now <- getPOSIXTime
                        writePrevTimes h (take 5 (realToFrac (now - started) : prev_times))
                    pure (stdoutTxt, code)

    withProgressBar title estimate $ \bar ->
        (fmap Right action `catch` \(ex :: SomeException) -> pure (Left ex)) >>= \case
            Left ex -> do
                completeProgressBar bar (format (s%" failed with exception: "%s%" ("%s%")") cmd (T.pack (show ex)) log_fp_txt) Red
                pure Nothing
            Right (_, ExitFailure n) -> do
                completeProgressBar bar (format (s%" failed with exit code: "%d%" ("%s%")") cmd n log_fp_txt) Red
                pure Nothing
            Right (result, ExitSuccess) -> do
                wrote_to_stderr <- readIORef wrote_to_stderr_ref
                let color = if wrote_to_stderr then Yellow else Green
                completeProgressBar bar (format (s%" completed ("%s%")") cmd log_fp_txt) color
                pure (Just result)

readPrevTimes :: Int -> IO [Double]
readPrevTimes h = handle (\(_ :: SomeException) -> pure []) $
    either (const []) id . decode <$> BS.readFile (fpToString (".tasks" </> hashToFilePath h </> "times"))

writePrevTimes :: Int -> [Double] -> IO ()
writePrevTimes h xs = BS.writeFile (fpToString (".tasks" </> hashToFilePath h </> "times")) (encode xs)

hashToFilePath :: Int -> FilePath
hashToFilePath = fromText . T.pack . show

fpToString :: FilePath -> String
fpToString path = let Right path' = toText path in T.unpack path'

withLogfile :: FilePath -> ((Handle, Text -> IO (), Text -> IO ()) -> IO r) -> IO r
withLogfile path act = do
    writeLock <- newMVar ()
    with (writeonly path) $ \log_handle -> do
        let logFrom :: Text -> Text -> IO ()
            logFrom typ line = withMVar writeLock $ \_ -> do
                now <- getCurrentTime
                let line' = format ("["%s%"] ["%s%"]: "%s) typ (T.pack (show now)) line
                outhandle log_handle (pure line')

        act (log_handle, logFrom "stdout", logFrom "stderr")
