{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Task where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.Managed
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as BS
import           Data.Function                (fix)
import           Data.Hashable
import           Data.IORef
import           Data.Serialize               (encode, decode)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import qualified Data.Text.IO                 as T
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           Data.Time.Format
import           GHC.Generics                 (Generic)
import           Prelude                      hiding (FilePath)
import           System.Console.ANSI
import           System.Console.AsciiProgress
import           System.Exit
import           System.IO                    (Handle, hClose)
import qualified System.Process               as Process
import           Text.Printf
import           Turtle

data Task = Task
    { taskInput   :: [Text]
    , taskCommand :: Text
    } deriving (Show, Generic, Hashable)

instance IsString Task where
    fromString s = Task [] (T.pack s)

data Result = Result
    { resultTask    :: Task
    , resultLogfile :: String
    , resultOutcome :: Either SomeException ExitCode
    } deriving Show

printResult :: Result -> IO ()
printResult Result{..} =
    case resultOutcome of
        Left ex -> do
            red
            echo (format (s%" failed with exception: "%s%" (log: "%s%")") (taskCommand resultTask) (T.pack (show ex)) (T.pack resultLogfile))
            reset
            exit (ExitFailure 1)
        Right (ExitFailure n) -> do
            red
            echo (format (s%" failed with exit code: "%d%" (log: "%s%")") (taskCommand resultTask) n (T.pack resultLogfile))
            reset
            exit (ExitFailure 1)
        Right ExitSuccess -> do
            green
            echo (format (s%" completed successfully (log: "%s%")") (taskCommand resultTask) (T.pack resultLogfile))
            reset
  where
    red   = setSGR [SetColor Foreground Vivid Red]
    green = setSGR [SetColor Foreground Dull Green]
    reset = setSGR [Reset]

tickLen = 0.10

runTask :: Task -> IO Result
runTask task = do
    (log_file, mestimate, taskAction) <- runTask_ task
    let estimate = maybe 60 id mestimate
    pg <- makeProgressBar estimate
    forkIO . fix $ \loop -> do
        done <- isComplete pg
        unless done $ do
            threadDelay (floor (tickLen * 1000 * 1000))
            tick pg
            loop

    result <- taskAction
    complete pg
    pure (Result task (fpToString log_file) result)
  where
    makeProgressBar :: Double -> IO ProgressBar
    makeProgressBar n =
        let name_width = 10 in
        newProgressBar def
            { pgFormat = T.unpack (T.justifyLeft name_width ' ' (T.take name_width (taskCommand task)))
                         ++ " :percent :bar| :eta remaining"
            , pgCompletedChar = '█'
            , pgTotal = floor (n / tickLen)
            , pgOnCompletion = T.putStr ((taskCommand task) <> " completed")
            }

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
        tryClose h = do
            first <- tryPutMVar hInLock ()
            when first (hClose h)

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
        close (hIn, hOut, hErr, ph) = do
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

                    feedIn `concurrently` feedOut `concurrently` feedErr

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
fpToString fp = let Right fp' = toText fp in T.unpack fp'

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
