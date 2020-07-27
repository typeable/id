module Data.Id
  ( Id(..)
  , coerceId
  ) where

import           Control.DeepSeq (NFData)
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Binary (Binary)
import           Data.Coerce (coerce)
import           Data.Hashable (Hashable)
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Database.PostgreSQL.Simple.ToField (ToField)
import           GHC.Generics (Generic)
import           Test.QuickCheck (Arbitrary)
import           Yesod.Core (PathPiece(..))


newtype Id (t :: k) = Id { unId :: UUID }
  deriving
  ( Eq, Ord, Generic, Read, Show, ToField, FromField, PathPiece, FromJSON
  , ToJSON, NFData, Hashable)

type role Id nominal

instance Binary (Id t) where

instance PathPiece UUID.UUID where
  fromPathPiece = UUID.fromText
  toPathPiece = UUID.toText

-- | This is a more \"explicit\" 'coerce' specifically for 'Id'.
-- You are forced to explicitly specify the phantom types you are converting
-- via the @TypeApplications@ compiler extension.
coerceId :: forall a b. Id (Ambiguous a) -> Id (Ambiguous b)
coerceId = coerce
{-# INLINE coerceId #-}

-- https://kcsongor.github.io/ambiguous-tags/
type family Ambiguous (a :: k) :: j where
  Ambiguous x = x
