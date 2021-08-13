{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Id
  ( Id(..)
  , coerceId
  , nilId
  , _Id
  ) where

import           Control.DeepSeq (NFData)
import           Control.Lens
import           Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           Data.Binary (Binary)
import           Data.Coerce (coerce)
import           Data.Data
import           Data.Hashable (Hashable)
import           Data.OpenApi
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
#ifndef ghcjs_HOST_OS
import           Database.PostgreSQL.Simple.FromField (FromField)
import           Database.PostgreSQL.Simple.ToField (ToField)
#endif
import           GHC.Generics (Generic)
import           Test.QuickCheck
import           Web.HttpApiData
import           Web.PathPieces (PathPiece(..))

newtype Id t = Id { unId :: UUID }
  deriving
  ( Eq, Ord, Generic, Read, Show, Data
#ifndef ghcjs_HOST_OS
  , ToField, FromField
#endif
  , PathPiece, FromJSON
  , ToJSON, NFData, Hashable, FromJSONKey, ToJSONKey, ToSchema, ToParamSchema
  , FromHttpApiData, ToHttpApiData )

type role Id nominal

instance Arbitrary (Id t) where
  arbitrary = fmap Id $ UUID.fromWords
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Binary (Id t) where

instance PathPiece UUID.UUID where
  fromPathPiece = UUID.fromText
  toPathPiece = UUID.toText

makePrisms ''Id

-- | This is a more \"explicit\" 'coerce' specifically for 'Id'.
-- You are forced to explicitly specify the phantom types you are converting
-- via the @TypeApplications@ compiler extension.
coerceId :: forall a b. Id (Ambiguous a) -> Id (Ambiguous b)
coerceId = coerce
{-# INLINE coerceId #-}

-- | Id corresponding to UUID nil. Useful for testing etc.
nilId :: forall a.  Id (Ambiguous a)
nilId = Id UUID.nil
{-# INLINE nilId #-}
-- https://kcsongor.github.io/ambiguous-tags/
type family Ambiguous (a :: k) :: j where
  Ambiguous x = x
