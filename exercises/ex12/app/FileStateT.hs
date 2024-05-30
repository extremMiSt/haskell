{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}

module FileStateT where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State
import System.IO
import Control.Monad.Writer.Class
import Control.Monad.Error
import Control.Monad.RWS
import Data.Map
import qualified Data.Map as Map
import Control.Exception

newtype FileStateT s m a = FileStateT (ReaderT FilePath m a)
    deriving newtype (Functor, Applicative, Monad, MonadIO, MonadTrans)
    deriving newtype (MonadError e)

runFileStateT :: FilePath -> FileStateT s m a -> m a
runFileStateT fp (FileStateT m) = runReaderT m fp

instance (MonadIO m, Read s, Show s) => MonadState s (FileStateT s m) where 
    get :: (MonadIO m, Read s) => FileStateT s m s
    get = do 
        fp <- FileStateT ask
        str <- liftIO $ readFile' fp
        pure $ read str
    put :: (MonadIO m, Read s, Show s) => s -> FileStateT s m ()
    put s = do
        fp <- FileStateT ask
        liftIO $ writeFile fp $ show s

instance MonadReader r m => MonadReader r (FileStateT s m) where
    ask :: MonadReader r m => FileStateT s m r
    ask = lift ask
    local :: MonadReader r m => (r -> r) -> FileStateT s m a -> FileStateT s m a
    local f (FileStateT m) = do
        fp <- FileStateT ask
        lift $ local f $ runReaderT m fp

instance MonadWriter w m => MonadWriter w (FileStateT s m) where
    tell :: MonadWriter w m => w -> FileStateT s m ()
    tell = lift . tell
    listen :: MonadWriter w m => FileStateT s m a -> FileStateT s m (a, w)
    listen (FileStateT m) = do
        fp <- FileStateT ask
        lift $ listen $ runReaderT m fp
    pass :: MonadWriter w m => FileStateT s m (a, w -> w) -> FileStateT s m a
    pass (FileStateT m) = do
        fp <- FileStateT ask
        lift $ pass $ runReaderT m fp

instance (MonadReader r m, MonadWriter w m, MonadIO m, Read s, Show s) => MonadRWS r w s (FileStateT s m)
        
memoizingFib :: MonadState (Map Integer Integer) m => Integer -> m Integer
memoizingFib 0 = pure 1
memoizingFib 1 = pure 1
memoizingFib n = do
    memo <- gets (Map.lookup n)
    case memo of 
        Just fib -> pure fib
        Nothing -> do
            x1 <- memoizingFib (n-1)
            x2 <- memoizingFib (n-2)
            modify' $ Map.insert n (x1+x2)
            pure (x1+x2)

memoizingFibSt :: Integer -> Integer
memoizingFibSt n = evalState (memoizingFib n) Map.empty

memoizingFibStIO :: FilePath -> Integer -> IO Integer
memoizingFibStIO fp n = runFileStateT fp do
    res <- liftIO ( try $ readFile' fp :: IO (Either IOError String))
    case res of
        Right _ -> pure ()
        Left e -> put Map.empty
    memoizingFib n

