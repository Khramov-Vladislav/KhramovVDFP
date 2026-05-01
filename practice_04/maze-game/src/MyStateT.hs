{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module MyStateT
    ( MyStateT(..)      
    , getMy
    , putMy
    , modifyMy
    ) where

import Control.Monad (ap)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.IO.Class (MonadIO(..))

newtype MyStateT s m a = MyStateT { runMyStateT :: s -> m (a, s) }

instance Functor m => Functor (MyStateT s m) where
    fmap f (MyStateT g) = MyStateT $ \s -> fmap (\(a, s') -> (f a, s')) (g s)

instance Monad m => Applicative (MyStateT s m) where
    pure a = MyStateT $ \s -> return (a, s)
    (<*>) = ap

instance Monad m => Monad (MyStateT s m) where
    return = pure
    (MyStateT g) >>= k = MyStateT $ \s -> do
        (a, s') <- g s
        runMyStateT (k a) s'

instance MonadTrans (MyStateT s) where
    lift ma = MyStateT $ \s -> do
        a <- ma
        return (a, s)

getMy :: Monad m => MyStateT s m s
getMy = MyStateT $ \s -> return (s, s)

putMy :: Monad m => s -> MyStateT s m ()
putMy s = MyStateT $ \_ -> return ((), s)

modifyMy :: Monad m => (s -> s) -> MyStateT s m ()
modifyMy f = getMy >>= putMy . f

instance MonadIO m => MonadIO (MyStateT s m) where
    liftIO = lift . liftIO