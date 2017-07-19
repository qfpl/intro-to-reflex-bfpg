{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Util.Attach (
    attachId
  ) where

import qualified Data.Text as T

import Reflex.Dom.Core
import GHCJS.DOM
import GHCJS.DOM.NonElementParentNode
import Language.Javascript.JSaddle.Monad (MonadJSM, liftJSM)

attachId ::
  MonadJSM  m =>
  T.Text ->
  (forall x. Widget x ()) ->
  m ()
attachId eid w =
  withJSContextSingleton $ \jsSing -> do
    doc <- currentDocumentUnchecked
    root <- getElementById doc eid
    case root of
      Nothing ->
        return ()
      Just docRoot ->
        liftJSM $ attachWidget docRoot jsSing w


