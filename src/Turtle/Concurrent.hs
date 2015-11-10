-- | A drop-in replacement for `Turtle`, with "thread-safe" echo/err. Currently
-- does not implement shell/proc with concurrent process handles.
--
-- Also exports Turtle.Task, for convenience.

{-# LANGUAGE OverloadedStrings #-}

module Turtle.Concurrent
    ( echo
    , err
    , stdout
    , stderr
    , module Turtle
    , module Turtle.Task
    ) where

import System.Console.Concurrent
import Turtle hiding (proc, shell, procStrict, shellStrict, echo, err, inproc, inshell, stdout, stderr)
import Turtle.Task

echo :: MonadIO m => Text -> m ()
echo txt = liftIO (outputConcurrent (txt <> "\n"))

err :: MonadIO m => Text -> m ()
err txt = liftIO (errorConcurrent (txt <> "\n"))

stdout :: MonadIO m => Shell Text -> m ()
stdout s = sh (s >>= echo)

stderr :: MonadIO m => Shell Text -> m ()
stderr s = sh (s >>= err)
