{-# LANGUAGE InstanceSigs #-}
module Main where
import qualified GADTs as G
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT), mapMaybeT)
import Control.Monad.Trans.Reader (Reader, ReaderT (runReaderT), runReader, ask, asks, mapReaderT)
import Control.Monad.Trans.State (StateT (runStateT), get, put)
import Control.Monad.Trans.Class (lift)
import Control.Applicative (Alternative(empty))
import Data.Functor.Identity (Identity(runIdentity))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Writer (Writer, runWriter, tell, censor, WriterT (runWriterT), pass)
import Prelude hiding (log)
import Data.Time (UTCTime, getCurrentTime)

type Password = String
newtype ProtectedData a = ProtectedData (Password -> Maybe a)
accessData :: Password -> ProtectedData a -> Maybe a
accessData pw (ProtectedData tryAccess) = undefined

type Protected s = MaybeT (Reader (ProtectedData s))

run :: ProtectedData s -> Protected s a -> Maybe a
run pd m = runReader (runMaybeT m) pd

access :: Password -> Protected s s
access pw = do
    pd <- lift ask
    let ms = accessData pw pd
    case ms of
        Nothing -> MaybeT $ return Nothing
        Just s -> return s

access' :: Password -> Protected s s
access' pw = MaybeT $ asks $ accessData pw

doubleAndShow :: Protected Integer String
doubleAndShow = do
    i <- access "Secret"
    return $ show (i*2)

type ProtectedIO s = StateT (Maybe Password) (MaybeT (ReaderT  (ProtectedData s) IO))

runIO :: ProtectedData s-> ProtectedIO s a -> IO (Maybe a) 
runIO pd m = do
    x <- runReaderT (runMaybeT $ runStateT m Nothing) pd
    return $ fmap fst x

embed :: Protected s a -> ProtectedIO s a
embed m = do
    pd <- lift $ lift ask
    let r = run pd m
    maybe empty return r

embed' :: Protected s a -> ProtectedIO s a
embed' = lift . mapMaybeT (mapReaderT (return . runIdentity))

accessIO :: ProtectedIO s s
accessIO  = do
    mpw <- get
    case mpw  of
        Just pw -> embed (access pw)
        Nothing -> do
            liftIO $ putStrLn "pw please"
            pw <- liftIO getLine
            put $ Just pw
            embed (access pw)

doubleAndShowIO :: ProtectedIO Integer String
doubleAndShowIO = do
    i <- accessIO
    return $ show (i*2)

---ex2---

data Item s m = Msg m | Section s [Item s m]
    deriving (Show)
type Log s m = [Item s m]
type Logging s m = Writer (Log s m)

-- Log a single message.
log :: m -> Logging s m ()
log m = tell [Msg m]

-- Group the nested messages in a section.
section :: s -> Logging s m a -> Logging s m a
section s = censor (\logItems -> [Section s logItems])

-- Extract the final result with the log messages.
runLogging :: Logging s m a -> (a, Log s m)
runLogging  = runWriter

loggingFib :: Integer -> Logging String String Integer
loggingFib 0 = log "Base case 0" >> return 1
loggingFib 1 = log "Base case 1" >> return 1
loggingFib n = section (show n) $ do
    n1 <- loggingFib (n-1)
    n2 <- loggingFib (n-2)
    return (n1+n2)

type StampedLog s m = [Item (UTCTime, s, UTCTime) (UTCTime, m)]

type StampedLogging s m = WriterT (StampedLog s m) IO

stampedLog :: m -> StampedLogging s m ()
stampedLog m = do
    t <- liftIO getCurrentTime
    tell [Msg (t, m)]
    
stampedSection :: s -> StampedLogging s m a -> StampedLogging s m a
stampedSection s m = pass $ do 
    start <- liftIO getCurrentTime
    a <- m
    end <- liftIO getCurrentTime
    return (a, \lm -> [Section (start, s, end) lm])

runStampedLog :: StampedLogging s m a -> IO (a, StampedLog s m)
runStampedLog = runWriterT

main :: IO ()
main = do 
    G.main
