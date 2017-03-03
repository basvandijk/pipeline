{-# LANGUAGE ScopedTypeVariables #-}

module Pipeline
    ( Handle
    , with, new, close
    , pipeline
    ) where

import Control.Concurrent ( ThreadId, forkIOWithUnmask, killThread )
import Control.Concurrent.STM.TBChan ( TBChan, newTBChanIO, readTBChan, writeTBChan )
import Control.Concurrent.MVar ( MVar, newMVar, withMVar, newEmptyMVar, putMVar, readMVar )
import Control.Monad.STM ( atomically )
import Control.Monad ( forever, join )
import Control.Exception ( bracket, try, throwIO, SomeException )

data Handle = Handle
    { writeLock      :: !(MVar ())
    , readResultChan :: !ReadResultChan
    , readDaemonTid  :: !ThreadId
    }

type ReadResultChan = TBChan (IO ())

with
    :: Int -- ^ Maximum number of pipelined requests
    -> (Handle -> IO a)
    -> IO a
with capacity = bracket (new capacity) close

new :: Int -- ^ Maximum number of pipelined requests
    -> IO Handle
new capacity = do
    lock <- newMVar ()
    chan <- newTBChanIO capacity
    tid <- forkIOWithUnmask $ \unmask -> unmask $ forever $
             join $ atomically $ readTBChan chan
    pure Handle
         { writeLock      = lock
         , readResultChan = chan
         , readDaemonTid  = tid
         }

close :: Handle -> IO ()
close = killThread . readDaemonTid

pipeline
    :: forall a
     . Handle
    -> IO () -- write request
    -> IO a  -- read response and parse it
    -> IO (IO a)
pipeline hndl performRequest getResponse = withMVar (writeLock hndl) $ \_ -> do
    performRequest

    responseVar <- newEmptyMVar

    let readResult :: IO ()
        readResult = do
          r <- try getResponse
          putMVar responseVar r

        getResult :: IO a
        getResult = do
          r <- readMVar responseVar
          case r of
            Left ex -> throwIO (ex :: SomeException)
            Right x -> pure x

    atomically $ writeTBChan (readResultChan hndl) readResult

    pure getResult
