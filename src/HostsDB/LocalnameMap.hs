module HostsDB.LocalnameMap
  ( LocalnameMap( LocalnameMap ), LocalNameRelation( lfrom, lto ), unLHMap )
where

-- aeson -------------------------------

import Data.Aeson.Key     ( toText )
import Data.Aeson.KeyMap  ( KeyMap, toList )
import Data.Aeson.Types   ( FromJSON, Value( Object, String ), typeMismatch )

-- base --------------------------------

import Control.Monad   ( fail, mapM, return )
import Data.Bifunctor  ( first )
import Data.Either     ( Either( Left, Right ), either )
import Data.Eq         ( Eq )
import Data.Function   ( ($) )
import Data.Functor    ( fmap )
import Data.Monoid     ( Monoid )
import Data.Tuple      ( uncurry )
import GHC.Generics    ( Generic )
import Text.Show       ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (∅), (⊕) )

-- containers-plus ---------------------

import ContainersPlus.Map  ( RepeatedKeyError, __fromList, fromList )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- dhall -------------------------------

import qualified  Dhall  as  D

import Dhall  ( FromDhall, Decoder, auto, autoWith, field, record )

-- domainnames -------------------------

import DomainNames.Error.LocalnameError  ( LocalnameError )
import DomainNames.Hostname              ( Localname, parseLocalname' )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element
                             , MonoFoldable( ofoldl', ofoldl1Ex', ofoldMap
                                           , ofoldr, ofoldr1Ex )
                             )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monad        ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- text --------------------------------

import Data.Text  ( Text, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- unordered-containers ----------------

import qualified  Data.HashMap.Strict  as  HashMap

-- yaml --------------------------------

import qualified  Data.Yaml  as  Yaml

--------------------------------------------------------------------------------

newtype LocalnameMap =
    LocalnameMap { unLHMap ∷ HashMap.HashMap Localname Localname }
  deriving (Eq, Generic, NFData, Show)

data LocalNameRelation =
    LocalNameRelation { lfrom ∷ Localname, lto ∷ Localname }
  deriving Eq

instance Printable LocalNameRelation where
  print (LocalNameRelation from to) = P.text $ [fmt|(%T → %T)|] from to

type instance Element LocalnameMap = LocalNameRelation
instance MonoFoldable LocalnameMap where
  ofoldMap ∷ Monoid ξ ⇒ (LocalNameRelation → ξ) → LocalnameMap → ξ
  ofoldMap f hm =
    HashMap.foldlWithKey' (\ a k v → a ⊕ f (LocalNameRelation k v)) ∅ hm

  ofoldr ∷ (LocalNameRelation → α → α) → α → LocalnameMap → α
  ofoldr f init (LocalnameMap hm) =
    HashMap.foldrWithKey (\ k v a → f (LocalNameRelation k v) a) init hm

  ofoldl' ∷ (α → LocalNameRelation → α) → α → LocalnameMap → α
  ofoldl' f init (LocalnameMap hm) =
    HashMap.foldlWithKey' (\ a k v → f a (LocalNameRelation k v)) init hm

  ofoldr1Ex ∷ (LocalNameRelation → LocalNameRelation → LocalNameRelation)
            → LocalnameMap → LocalNameRelation
  ofoldr1Ex f (LocalnameMap hm) =
    ofoldr1Ex f (uncurry LocalNameRelation ⊳ HashMap.toList hm)

  ofoldl1Ex' ∷ (LocalNameRelation → LocalNameRelation → LocalNameRelation)
             → LocalnameMap → LocalNameRelation
  ofoldl1Ex' f (LocalnameMap hm) =
    ofoldl1Ex' f (uncurry LocalNameRelation ⊳ HashMap.toList hm)

data LocalAlias = LocalAlias Localname Localname

localAliasType ∷ Decoder LocalAlias
localAliasType = record $ LocalAlias ⊳ field "from" auto
                                     ⊵ field "to"   auto

instance FromDhall LocalAlias where
  autoWith _ = localAliasType

localAliasPair ∷ LocalAlias → (Localname,Localname)
localAliasPair (LocalAlias aliasFrom aliasTo) = (aliasFrom,aliasTo)

localHostMapType ∷ Decoder LocalnameMap
localHostMapType =
  LocalnameMap ⊳ __fromList ∘ fmap localAliasPair ⊳ D.list localAliasType

instance FromDhall LocalnameMap where
  autoWith _ = localHostMapType

instance FromJSON LocalnameMap where
  parseJSON (Object hm) =
    let go' ∷ MonadError LocalnameError η ⇒
              (Text,Text) → η (Localname,Localname)
        go' (k, v) = do k' ← parseLocalname' k
                        v' ← parseLocalname' v
                        return (k',v')
        go ∷ (Text,Value) → Yaml.Parser (Localname, Localname)
        go (k,String v) = either (fail ∘ toString) return $ go' (k,v)
        go (k,invalid)  =
          typeMismatch (unpack $ "local host name: '" ⊕ k ⊕ "'") invalid
        toListT ∷ KeyMap Value → [(Text,Value)]
        toListT km = first toText ⊳ toList km

     in fromList @(RepeatedKeyError Localname) ⊳ (mapM go $ toListT hm) ≫ \ case
          Left dups → fail $ toString dups
          Right hm' → return $ LocalnameMap hm'
  parseJSON invalid     = typeMismatch "local host map" invalid

-- that's all, folks! ----------------------------------------------------------
