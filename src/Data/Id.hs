{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Id
  ( Id, mkId, unId, randomId, unsafeIdTagConvert, coerceId, _Id, nilId
  , IntId, mkIntId, unIntId, unsafeIntIdTagConvert, coerceIntId, _IntId
  , Name, mkName, unName, unsafeNameTagConvert, coerceName, _Name
  )
  where

import           Control.DeepSeq (NFData)
import           Control.Lens
import           Control.Monad
import           Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import           Data.Binary (Binary)
import           Data.Coerce (coerce)
import           Data.Data
import           Data.Hashable (Hashable)
import           Data.OpenApi
import           Data.String
import           Data.Text as T
import           Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import           GHC.TypeLits
import           Prelude as P
import           Test.QuickCheck
import           Text.ParserCombinators.ReadP
import           Web.HttpApiData
import           Web.PathPieces (PathPiece(..))

#ifdef USE_CASSAVA
import           Data.Csv as Csv hiding(Name)
#endif
#ifdef USE_POSTGRES
import           Database.PostgreSQL.Simple.FromField as PG (FromField)
import           Database.PostgreSQL.Simple.ToField as PG (ToField)
#endif

#ifdef USE_FLAT
import           Flat as F
#endif
#ifdef USE_STORE
import qualified Data.Store
#endif

newtype Id t = Id { unId :: UUID }
  deriving stock Data
  deriving newtype
  ( Eq, Ord, Binary
#ifndef ghcjs_HOST_OS
#ifdef USE_POSTGRES
  , PG.ToField, PG.FromField
#endif
#endif
  , FromJSON, ToJSON, NFData, Hashable, FromJSONKey, ToJSONKey, ToSchema
  , ToParamSchema, FromHttpApiData, ToHttpApiData
  )

type role Id nominal

makePrisms ''Id

mkId :: forall s. UUID -> Id s
mkId = coerce
{-# INLINE mkId #-}

randomId :: forall s. IO (Id s)
randomId = coerce <$> UUID.V4.nextRandom
{-# INLINE randomId #-}

instance KnownSymbol s => Show (Id s) where
  show (Id v) = "Id-" <> symbolVal (Proxy @s) <> "-" <> P.show v

instance KnownSymbol s => Read (Id s) where
  readsPrec _ = readP_to_S $ do
    _ <- string ("Id-" <> symbolVal (Proxy @s) <> "-")
    mkId <$> readS_to_P reads

#ifdef USE_FLAT
instance Flat (Id s) where
  encode = F.encode . UUID.toWords . coerce
  decode = coerce . (\(a,b,c,d) -> UUID.fromWords a b c d) <$> F.decode
  size = F.size . UUID.toWords . coerce
#endif

#ifdef USE_STORE
instance Data.Store.Store (Id s) where
  poke = Data.Store.poke . UUID.toWords . coerce
  peek = coerce . (\(a,b,c,d) -> UUID.fromWords a b c d) <$> Data.Store.peek
  size = Data.Store.ConstSize 16
#endif

instance Arbitrary (Id t) where
  arbitrary = fmap Id $ UUID.fromWords
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance PathPiece (Id t) where
  fromPathPiece = coerce . UUID.fromText
  toPathPiece = UUID.toText . coerce

#ifndef ghcjs_HOST_OS

#ifdef USE_CASSAVA
instance Csv.FromField (Id tag) where
  parseField = parseField
    >=> fmap mkId . maybe (fail "invalid UUID in CSV") pure . UUID.fromText

instance Csv.ToField (Id tag) where
  toField = Csv.toField . UUID.toText . coerce
#endif

#endif

-- | This is a more \"explicit\" 'coerce' specifically for 'Id'.
-- You are forced to explicitly specify the phantom types you are converting
-- via the @TypeApplications@ compiler extension.
coerceId, unsafeIdTagConvert :: forall a b. Id (Ambiguous a) -> Id (Ambiguous b)
coerceId = coerce
{-# INLINE coerceId #-}
unsafeIdTagConvert = coerce
{-# INLINE unsafeIdTagConvert #-}

-- | Id corresponding to UUID nil. Useful for testing etc.
nilId :: forall a.  Id (Ambiguous a)
nilId = Id UUID.nil
{-# INLINE nilId #-}
-- https://kcsongor.github.io/ambiguous-tags/
type family Ambiguous (a :: k) :: j where
  Ambiguous x = x

----------------- IntId
newtype IntId t = IntId { unIntId :: Integer }
  deriving stock Data
  deriving newtype
  ( Eq, Ord, Binary
#ifndef ghcjs_HOST_OS
#ifdef USE_POSTGRES
  , PG.ToField, PG.FromField
#endif
#ifdef USE_CASSAVA
  , Csv.ToField, Csv.FromField
#endif
#endif
#ifdef USE_FLAT
  , Flat
#endif
#ifdef USE_STORE
  , Data.Store.Store
#endif
  , ToSchema, ToParamSchema, Arbitrary, FromJSON, ToJSON, NFData, Hashable
  , FromJSONKey, ToJSONKey, FromHttpApiData, ToHttpApiData, PathPiece, Num
  , Integral, Real, Enum
  )

type role IntId nominal

makePrisms ''IntId

mkIntId :: forall s. Integer -> IntId s
mkIntId = coerce
{-# INLINE mkIntId #-}

instance KnownSymbol s => Show (IntId s) where
  show (IntId v) = "IntId-" <> symbolVal (Proxy @s) <> "-" <> P.show v

instance KnownSymbol s => Read (IntId s) where
  readsPrec _ = readP_to_S $ do
    _ <- string ("IntId-" <> symbolVal (Proxy @s) <> "-")
    mkIntId <$> readS_to_P reads

-- | This is a more \"explicit\" 'coerce' specifically for 'IntId'.
-- You are forced to explicitly specify the phantom types you are converting
-- via the @TypeApplications@ compiler extension.
coerceIntId, unsafeIntIdTagConvert
  :: forall a b. IntId (Ambiguous a) -> IntId (Ambiguous b)
coerceIntId = coerce
{-# INLINE coerceIntId #-}
unsafeIntIdTagConvert = coerce
{-# INLINE unsafeIntIdTagConvert #-}

----------------- Name
newtype Name t = Name { unName :: Text }
  deriving stock Data
  deriving newtype
  ( Eq, Ord, Binary
#ifndef ghcjs_HOST_OS
#ifdef USE_POSTGRES
  , PG.ToField, PG.FromField
#endif
#ifdef USE_CASSAVA
  , Csv.ToField, Csv.FromField
#endif
#endif
#ifdef USE_FLAT
  , Flat
#endif
#ifdef USE_STORE
  , Data.Store.Store
#endif
  , ToSchema, ToParamSchema, FromJSON, ToJSON, NFData, Hashable, FromJSONKey
  , ToJSONKey, FromHttpApiData, ToHttpApiData, PathPiece, IsString, Semigroup
  , Monoid
  )

type role Name nominal

makePrisms ''Name

mkName :: forall s. Text -> Name s
mkName = coerce
{-# INLINE mkName #-}

instance KnownSymbol s => Show (Name s) where
  show (Name v) = "Name-" <> symbolVal (Proxy @s) <> "-" <> P.show v

instance KnownSymbol s => Read (Name s) where
  readsPrec _ = readP_to_S $ do
    _ <- string ("Name-" <> symbolVal (Proxy @s) <> "-")
    mkName <$> readS_to_P reads

instance Arbitrary (Name s) where
  arbitrary = coerce . T.pack . getPrintableString <$> arbitrary

-- | This is a more \"explicit\" 'coerce' specifically for 'Name'.
-- You are forced to explicitly specify the phantom types you are converting
-- via the @TypeApplications@ compiler extension.
coerceName, unsafeNameTagConvert
  :: forall a b. Name (Ambiguous a) -> Name (Ambiguous b)
coerceName = coerce
{-# INLINE coerceName #-}
unsafeNameTagConvert = coerce
{-# INLINE unsafeNameTagConvert #-}
