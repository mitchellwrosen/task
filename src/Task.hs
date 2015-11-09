{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task
    ( Task(..)
    , runTask
    ) where

import ProgressBar

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Control.Monad.Managed
import qualified Data.ByteString              as BS
import           Data.Hashable
import           Data.Serialize               (encode, decode)
import qualified Data.Text                    as T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           GHC.Generics                 (Generic)
import           Prelude                      hiding (FilePath)
import           System.Exit
import           System.IO                    (hClose)
import qualified System.Process               as Process
import           Turtle

data Task = Task
    { taskInput   :: [Text]
    , taskCommand :: Text
    } deriving (Show, Generic, Hashable)

instance IsString Task where
    fromString str = Task [] (T.pack str)

-- | Run a task and return whether or not it failed.
runTask :: Task -> IO Bool
runTask task = do
    (log_file, mestimate, taskAction) <- runTask_ task

    let log_file_txt = T.pack (fpToString log_file)
        estimate = maybe 60 id mestimate

    withProgressBar (taskCommand task) estimate $ \bar ->
        taskAction >>= \case
            Left ex -> do
                crashProgressBar bar (format (s%" failed with exception: "%s%" (log: "%s%")") (taskCommand task) (T.pack (show ex)) log_file_txt)
                pure False
            Right (ExitFailure n) -> do
                crashProgressBar bar (format (s%" failed with exit code: "%d%" (log: "%s%")") (taskCommand task) n log_file_txt)
                pure False
            Right ExitSuccess -> do
                completeProgressBar bar (format (s%" completed (log: "%s%")") (taskCommand task) log_file_txt)
                pure True

runTask_ :: Task -> IO (FilePath, Maybe Double, IO (Either SomeException ExitCode))
runTask_ task@(Task inp cmd) = do
    let h = hash task

    mktree (".tasks" </> hashToFilePath h </> "logs")

    prev_times <- readPrevTimes h
    let estimate =
            case prev_times of
                [] -> Nothing
                _  -> Just (sum prev_times / fromIntegral (length prev_times))

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

    now_fp <- fromText . T.pack . formatTime defaultTimeLocale "%F_%X%Q" <$> getCurrentTime
    let log_fp = ".tasks" </> hashToFilePath h </> "logs" </> now_fp

        action :: IO ExitCode
        action = do
            withLogfile log_fp $ \(log_handle, logStdout, logStderr) -> do
                outhandle log_handle (pure ("Command: " <> taskCommand task) <|> "Stdin lines:" <|> select (taskInput task) <|> "--------------------")

                bracket open close $ \(hIn, hOut, hErr, ph) -> do
                    let feedIn :: IO ()
                        feedIn = do
                            outhandle hIn (select inp)
                            tryClose hIn

                        feedOut :: IO ()
                        feedOut = sh (inhandle hOut >>= liftIO . logStdout)

                        feedErr :: IO ()
                        feedErr = sh (inhandle hErr >>= liftIO . logStderr)

                    started <- getPOSIXTime

                    _ <- feedIn `concurrently` feedOut `concurrently` feedErr

                    code <- Process.waitForProcess ph
                    when (code == ExitSuccess) $ do
                        now <- getPOSIXTime
                        writePrevTimes h (take 5 (realToFrac (now - started) : prev_times))
                    pure code

    pure (log_fp, estimate, fmap Right action `catch` (pure . Left))

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
