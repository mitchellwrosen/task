{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Turtle.Task
    ( Task(..)
    , task
    , taskStrict
    ) where

import Turtle.ProgressBar

import           Control.Concurrent.Async
import qualified Control.Concurrent.Lock   as Lock
import           Control.Exception
import           Control.Monad
import           Control.Monad.Managed
import qualified Data.ByteString           as BS
import           Data.Hashable
import           Data.IORef
import           Data.Maybe
import           Data.Serialize            (encode, decode)
import qualified Data.Text                 as T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           Prelude                   hiding (FilePath, log)
import           System.Console.ANSI
import           System.Console.Concurrent
import           System.Exit
import           System.IO                 (hClose)
import qualified System.Process            as Process
import           System.Random
import           Turtle

data Task = Task
    { taskCommand          :: Text       -- ^ Command to run.
    , taskInput            :: Shell Text -- ^ Standard input.
    , taskProgBarTitle     :: Text       -- ^ Progress bar title.
    , taskProgBarDisappear :: Bool       -- ^ Disappear after successful run?
    , taskLog              :: Bool       -- ^ Log stdout/stderr?
    , taskVerbose          :: Bool       -- ^ Print stdout/stderr?
    }

-- | Run a task and return whether or not it succeeded.
task :: Task -> IO Bool
task t = isJust <$> task_ sh t

-- | Run a task and return its stdout (if successful).
taskStrict :: Task -> IO (Maybe Text)
taskStrict = task_ strict

task_ :: forall a.
         (Shell Text -> IO a)
      -> Task
      -> IO (Maybe a)
task_ onStdout (Task cmd stdin_shell pb_title pb_disappear do_log verbose) = do
    stdin_lines <- shellToList stdin_shell

    let h = hash (cmd, stdin_lines)

    makeTasksDirs h

    prev_times <- readPrevTimes h

    hInLock <- Lock.new

    let tryClose :: Handle -> IO ()
        tryClose hdl = do
            first <- Lock.tryAcquire hInLock
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

    now_file_path <- fromText . T.pack . formatTime defaultTimeLocale "%F_%X%Q" <$> getCurrentTime
    let log_file_path = ".tasks" </> hashToFilePath h </> "logs" </> now_file_path

        withMaybeLogFile =
            if do_log
                then withLogFile log_file_path
                else withNoLogFile

        action :: IO (a, ExitCode)
        action = withMaybeLogFile $ \maybe_log_file -> do
            logRaw maybe_log_file $
                    pure ("Command: " <> cmd)
                <|> "Stdin lines:"
                <|> stdin_shell
                <|> "--------------------"

            bracket open close $ \(hIn, hOut, hErr, ph) -> do

                let feedIn :: IO ()
                    feedIn = do
                        outhandle hIn (select stdin_lines)
                        tryClose hIn

                    feedOut :: IO a
                    feedOut = onStdout $ do
                        txt <- inhandle hOut
                        liftIO (logStdout maybe_log_file txt)
                        when verbose
                            (liftIO (outputConcurrent (txt <> "\n")))
                        pure txt

                    feedErr :: IO ()
                    feedErr = sh $ do
                        txt <- inhandle hErr
                        liftIO (writeIORef wrote_to_stderr_ref True)
                        liftIO (logStderr maybe_log_file txt)
                        when verbose
                            (liftIO (errorConcurrent (txt <> "\n")))

                started <- getPOSIXTime

                ((_, result), _) <- feedIn `concurrently` feedOut `concurrently` feedErr

                code <- Process.waitForProcess ph
                when (code == ExitSuccess) $ do
                    now <- getPOSIXTime
                    writePrevTimes h (take 5 (realToFrac (now - started) : prev_times))
                pure (result, code)

    estimate <-
        case prev_times of
            [] -> liftIO (randomRIO (10, 30))
            _  -> pure (sum prev_times / fromIntegral (length prev_times))

    withProgressBar pb_title estimate $ \bar ->
        catchAll action >>= \case
            Left ex -> do
                let msg_txt =
                        if do_log
                            then format (s%": "%s%" ("%fp%")") pb_title (tshow ex) log_file_path
                            else format (s%": "%s) pb_title (tshow ex)

                completeProgressBar bar (Just (msg_txt, Red))
                pure Nothing

            Right (_, ExitFailure n) -> do
                let msg_txt =
                        if do_log
                            then format (s%": "%d%" ("%fp%")") pb_title n log_file_path
                            else format (s%": "%d) pb_title n

                completeProgressBar bar (Just (msg_txt, Red))
                pure Nothing

            Right (result, ExitSuccess) -> do
                wrote_to_stderr <- readIORef wrote_to_stderr_ref

                let msg_txt =
                        if do_log
                            then format (s%" ("%fp%")") pb_title log_file_path
                            else pb_title

                    msg = case (wrote_to_stderr, pb_disappear) of
                              (True, _) -> Just (msg_txt, Yellow)
                              (_, False) -> Just (msg_txt, Green)
                              _ -> Nothing

                completeProgressBar bar msg
                pure (Just result)
  where
    makeTasksDirs :: Int -> IO ()
    makeTasksDirs h =
        if do_log
            then mktree (".tasks" </> hashToFilePath h </> "logs")
            else mktree (".tasks" </> hashToFilePath h)

readPrevTimes :: Int -> IO [Double]
readPrevTimes h = handle (\(_ :: SomeException) -> pure []) $
    either (const []) id . decode <$> BS.readFile (fpToString (".tasks" </> hashToFilePath h </> "times"))

writePrevTimes :: Int -> [Double] -> IO ()
writePrevTimes h xs = BS.writeFile (fpToString (".tasks" </> hashToFilePath h </> "times")) (encode xs)

hashToFilePath :: Int -> FilePath
hashToFilePath = fromText . T.pack . show

fpToString :: FilePath -> String
fpToString path = let Right path' = toText path in T.unpack path'

catchAll :: IO a -> IO (Either SomeException a)
catchAll action = catch (Right <$> action) (pure . Left)

shellToList :: Shell a -> IO [a]
shellToList inp = fold inp (Fold (\k a -> \as -> k (a:as)) id (\k -> k []))

tshow :: Show a => a -> Text
tshow = T.pack . show

--------------------------------------------------------------------------------

type MaybeLogFile = Maybe (Handle, Text -> IO (), Text -> IO ())

withLogFile :: FilePath -> (MaybeLogFile -> IO r) -> IO r
withLogFile path act = do
    writeLock <- Lock.new

    with (writeonly path) $ \log_handle -> do
        let logFrom :: Text -> Text -> IO ()
            logFrom typ line = Lock.with writeLock $ do
                now <- getCurrentTime
                let line' = format ("["%s%"] ["%s%"]: "%s) typ (T.pack (show now)) line
                outhandle log_handle (pure line')

        act (Just (log_handle, logFrom "stdout", logFrom "stderr"))

withNoLogFile :: (MaybeLogFile -> IO r) -> IO r
withNoLogFile act = act Nothing

-- | Not safe to mix with other logging (doesn't acquire log lock). Meant for
-- log file "headers".
logRaw :: MonadIO m => MaybeLogFile -> Shell Text -> m ()
logRaw Nothing _ = pure ()
logRaw (Just (h, _, _)) msgs = outhandle h msgs

logStdout :: MaybeLogFile -> Text -> IO ()
logStdout Nothing _ = pure ()
logStdout (Just (_, k, _)) msg = k msg

logStderr :: MaybeLogFile -> Text -> IO ()
logStderr Nothing _ = pure ()
logStderr (Just (_, _, k)) msg = k msg
