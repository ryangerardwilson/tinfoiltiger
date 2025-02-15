{-# LANGUAGE OverloadedStrings #-}

module TinFoilTiger.Utils.MonadManager
  ( convert_action_in_any_monad_to_MaybeT_m_a,
  )
where

import Control.Monad.Trans.Maybe (MaybeT (..))

-- |
--  convert_IO_to_MaybeT_IO_a converts an action in any monad m (returning Either e (Maybe a))
--  to a MaybeT m a. This lets you plug in an IO (Either e (Maybe a)) or any other similar action,
--  as long as m is a Monad.
convert_action_in_any_monad_to_MaybeT_m_a :: (Monad m) => m (Either e (Maybe a)) -> MaybeT m a
convert_action_in_any_monad_to_MaybeT_m_a action = MaybeT $ do
  res <- action
  case res of
    Right (Just x) -> return (Just x)
    _ -> return Nothing
