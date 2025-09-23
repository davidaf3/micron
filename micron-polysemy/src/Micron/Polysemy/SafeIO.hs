{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Micron.Polysemy.SafeIO (SafeIO, performSafeIO, ioToSafeIO, runSafeIO) where

import Control.Exception (SomeException)
import Data.Kind (Type)
import Polysemy (Embed, Members, Sem, interpret, makeSem, reinterpret2)
import Polysemy.Error (Error, fromException)

data SafeIO (m :: Type -> Type) a where
  PerformSafeIO :: IO a -> SafeIO m a

makeSem ''SafeIO

runSafeIO ::
  (Members '[Embed IO, Error SomeException] r) =>
  Sem (SafeIO ': r) a ->
  Sem r a
runSafeIO = interpret $ \case
  PerformSafeIO run -> fromException @SomeException run

ioToSafeIO ::
  Sem (SafeIO ': r) a ->
  Sem (Error SomeException ': Embed IO ': r) a
ioToSafeIO = reinterpret2 $ \case
  PerformSafeIO run -> fromException @SomeException run
