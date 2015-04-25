{-# LANGUAGE ExplicitNamespaces #-}
module Woroni.Prelude (module X) where
--------------------------------------------------------------------------------
import Control.Applicative    as X
import Control.Arrow          as X
import Control.Lens           as X hiding (coerce)
import Control.Monad          as X hiding (forM, forM_, mapM, mapM_, msum,
                                    sequence, sequence_)
import Control.Monad.IO.Class as X
import Control.Monad.Trans    as X
--------------------------------------------------------------------------------
import Data.Coerce as X
import Data.Monoid as X
--------------------------------------------------------------------------------
import Data.ByteString    as X (ByteString)
import Data.String        as X
import Data.Text          as X (Text)
import Data.Text.Encoding as X (decodeUtf8, encodeUtf8)
--------------------------------------------------------------------------------
import Data.Int         as X
import Data.IORef       as X
import Data.Time        as X
import Data.Time.Format as X
import Data.Vector      as X (Vector)
import Data.Word        as X
-------------------------------------------------------------------------------j
import Data.Foldable    as X
import Data.Functor     as X
import Data.Maybe       as X
import Data.Traversable as X
import GHC.Generics     as X hiding (from, to)
import Prelude          as X hiding (all, and, any, concat, concatMap, elem,
                              foldl, foldl1, foldr, foldr1, map, mapM, mapM_,
                              maximum, minimum, notElem, or, product, sequence,
                              sequence, sequence_, sum)
--------------------------------------------------------------------------------
import System.Locale as X (defaultTimeLocale)

