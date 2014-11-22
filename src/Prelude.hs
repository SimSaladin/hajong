{-# LANGUAGE NoImplicitPrelude #-}
------------------------------------------------------------------------------
-- | 
-- Module         : Prelude
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Prelude (module Prelude, module X) where

------------------------------------------------------------------------------
import           ClassyPrelude                as X hiding (Index, (<.>), uncons, unsnoc, cons, snoc)
import           Control.Applicative          as X
import           Control.Lens                 as X
import           Control.Monad.Error.Class    as X
import           Control.Monad.Reader.Class   as X
import           Control.Monad.State.Class    as X
import           Control.Monad.Writer.Class   as X
import           Control.Monad.Trans.Either   as X

------------------------------------------------------------------------------
import qualified Text.PrettyPrint.ANSI.Leijen as P

------------------------------------------------------------------------------

type CanError = MonadError Text

if' :: Bool -> t -> t -> t
if' cond th el = if cond then th else el

(?) :: Monad m => Maybe a -> e -> EitherT e m a
mr ? err = maybe (left err) return mr

liftE :: CanError m => Either Text a -> m a
liftE = either throwError return

rview :: (MonadReader s m, MonadIO m) => Getting (TVar b) s (TVar b) -> m b
rview l = view l >>= atomically . readTVar

rswap :: (MonadReader s m, MonadIO m) => Getting (TVar b) s (TVar b) -> b -> m b 
rswap l a = view l >>= atomically . (`swapTVar` a)

rmodify :: (MonadReader s m, MonadIO m) => Getting (TVar a) s (TVar a) -> (a -> a) -> m ()
rmodify l f = view l >>= atomically . (`modifyTVar` f)

prettyList' :: P.Pretty a => [a] -> P.Doc
prettyList' = foldr ((P.<+>) . P.pretty) P.empty
