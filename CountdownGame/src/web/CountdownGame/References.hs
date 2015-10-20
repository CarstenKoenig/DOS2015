module CountdownGame.References
       ( Reference
       , createRef
       , readRef
       , modifyRef
       , clearRef
       ) where

import Data.IORef (IORef(..), newIORef, readIORef, atomicModifyIORef')

newtype Reference a = Reference { refOf :: IORef a }

createRef :: a -> IO (Reference a)
createRef a = Reference <$> newIORef a

readRef :: (a -> b) -> Reference a -> IO b
readRef f ref = f <$> readIORef (refOf ref)

modifyRef :: (a -> (a, b)) -> Reference a -> IO b
modifyRef f ref = atomicModifyIORef' (refOf ref) f

clearRef :: Reference (Maybe a) -> IO ()
clearRef = modifyRef (const (Nothing, ()))
