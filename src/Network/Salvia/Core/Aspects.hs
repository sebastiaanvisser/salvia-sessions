{-# LANGUAGE UndecidableInstances #-}
module Network.Salvia.Core.Aspects where

import Control.Applicative
import Control.Monad.State
import Network.Protocol.Http
import Network.Salvia.Core.Config
import Network.Socket
import System.IO
import qualified Data.ByteString.Lazy as B

forRequest :: Request
forRequest = undefined

forResponse :: Response
forResponse = undefined

class (Applicative m, Monad m) => HttpM d m where
  http :: State (Http d) a -> m a

class (HttpM Request m, HttpM Response m) => HttpM' m
instance (HttpM Request m, HttpM Response m) => HttpM' m

request :: HttpM Request m => State (Http Request) a -> m a
request = http

response :: HttpM Response m => State (Http Response) a -> m a
response = http

class (Applicative m, Monad m) => PeerM m where
  rawSock :: m Socket
  sock    :: m Handle
  peer    :: m SockAddr

-- TODO:  queue and dequeue are probably enough.
type SendAction = (Socket, Handle) -> IO ()

class (Applicative m, Monad m) => QueueM m where
  enqueue  :: SendAction -> m ()
  dequeue  :: m (Maybe SendAction)

  sendStr  :: String                                   -> m ()
  sendBs   :: B.ByteString                             -> m ()
  spoolStr :: (String       -> String)       -> Handle -> m ()
  spoolBs  :: (B.ByteString -> B.ByteString) -> Handle -> m ()

class (Applicative m, Monad m) => FlushM d m where
  flushHeaders  :: d -> m ()
  flushQueue    :: d -> m ()

class (Applicative m, Monad m) => BodyM d m where
  body :: d -> m (Maybe B.ByteString)

class (Applicative m, Monad m) => ServerM m where
  server :: m Config

class (Applicative m, Monad m) => PayloadM m p where
  payload :: State p a -> m a

class (Applicative m, Monad m) => ClientM m where
  client :: m ()

