{-# LANGUAGE NoImplicitPrelude #-}
------------------------------------------------------------------------------
-- |
-- Module         : Import
-- Copyright      : (C) 2014 Samuli Thomasson
-- License        : MIT (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module Import (module Import, module X) where

------------------------------------------------------------------------------
import           ClassyPrelude                as X hiding (Index, index, (<.>), uncons, unsnoc, cons, snoc)
import           Control.Applicative          as X
import           Control.Lens                 as X
import           Control.Monad.Error.Class    as X
import           Control.Monad.Reader.Class   as X
import           Control.Monad.State.Class    as X
import           Control.Monad.Writer.Class   as X
import           Control.Monad.Trans.Either   as X
import           Data.Aeson.TH
------------------------------------------------------------------------------
import qualified Text.PrettyPrint.ANSI.Leijen as P
import           Text.PrettyPrint.ANSI.Leijen as X (Pretty(..), (<+>), string)
------------------------------------------------------------------------------

type CanError m = (MonadError Text m, Functor m)

if' :: Bool -> t -> t -> t
if' cond th el = if cond then th else el

(?) :: Monad m => Maybe a -> e -> EitherT e m a
mr ? err = maybe (left err) return mr

(?!) :: Maybe a -> String -> a
mr ?! err = fromMaybe (error ("Game bug: was not Just: " ++ err)) mr

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

aesonOptions :: Int -> Options
aesonOptions n = defaultOptions
    { fieldLabelModifier     = unpack . toLower . asText . pack . drop n
    , constructorTagModifier = unpack . toLower . asText . pack
    , sumEncoding            = TaggedObject "type" "contents"
    }
