{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module HostsDB.HostMap
  ( HostMap( HostMap ), HostRelation( HostRelation, hrkey, hrvalue )
  , hmHosts, leftFail, returnPair, unHostMap )
where

-- aeson -------------------------------

import Data.Aeson.Key     ( toText )
import Data.Aeson.KeyMap  ( KeyMap, toList )
import Data.Aeson.Types   ( FromJSON, Value( Object ), parseJSON, typeMismatch )

-- base --------------------------------

import Control.Monad   ( fail, mapM, return )
import Data.Bifunctor  ( first )
import Data.Either     ( Either( Left, Right ) )
import Data.Eq         ( Eq )
import Data.Function   ( ($) )
import Data.Functor    ( fmap )
import Data.Monoid     ( Monoid )
import Data.Tuple      ( uncurry )
import Text.Show       ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (∅), (⊕) )

-- containers-plus ---------------------

import ContainersPlus.Map  ( RepeatedKeyError, __fromList, fromList )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString )

-- dhall -------------------------------

import qualified  Dhall  as  D
import Dhall  ( FromDhall( autoWith ), Decoder )

-- domainnames -------------------------

import DomainNames.Hostname  ( Hostname, parseHostname' )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element
                             , MonoFoldable( ofoldl', ofoldl1Ex', ofoldMap
                                           , ofoldr, ofoldr1Ex )
                             )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣) )
import Data.MoreUnicode.Monad    ( (≫) )

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

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import HostsDB.Host  ( Host, hname, hostType, leftFail, returnPair )

--------------------------------------------------------------------------------

newtype HostMap = HostMap { unHostMap ∷ HashMap.HashMap Hostname Host }
  deriving (Eq, Show)

data HostRelation = HostRelation { hrkey ∷ Hostname, hrvalue ∷ Host }
  deriving Eq

instance Printable HostRelation where
  print (HostRelation nm hst) = P.text $ [fmt|(%T → %T)|] nm hst

type instance Element HostMap = HostRelation
instance MonoFoldable HostMap where
  ofoldMap ∷ Monoid ξ ⇒ (HostRelation → ξ) → HostMap → ξ
  ofoldMap f hm =
    HashMap.foldlWithKey' (\ a k v → a ⊕ f (HostRelation k v)) ∅ hm

  ofoldr ∷ (HostRelation → α → α) → α → HostMap → α
  ofoldr f init (HostMap hm) =
    HashMap.foldrWithKey (\ k v a → f (HostRelation k v) a) init hm

  ofoldl' ∷ (α → HostRelation → α) → α → HostMap → α
  ofoldl' f init (HostMap hm) =
    HashMap.foldlWithKey' (\ a k v → f a (HostRelation k v)) init hm

  ofoldr1Ex ∷ (HostRelation → HostRelation → HostRelation)
            → HostMap → HostRelation
  ofoldr1Ex f (HostMap hm) =
    ofoldr1Ex f (uncurry HostRelation ⊳ HashMap.toList hm)

  ofoldl1Ex' ∷ (HostRelation → HostRelation → HostRelation)
             → HostMap → HostRelation
  ofoldl1Ex' f (HostMap hm) =
    ofoldl1Ex' f (uncurry HostRelation ⊳ HashMap.toList hm)


instance FromJSON HostMap where
  parseJSON (Object hm) =
    let go ∷ (Text,Value) → Yaml.Parser (Hostname, Host)
        go (k,v@(Object _)) = returnPair (leftFail $ parseHostname' k, parseJSON v)
        go (k,invalid)  =
          typeMismatch (unpack $ "Host: '" ⊕ k ⊕ "'") invalid
        toListT ∷ KeyMap Value → [(Text,Value)]
        toListT km = first toText ⊳ toList km

     in fromList @(RepeatedKeyError Hostname) ⊳ mapM go (toListT hm) ≫ \ case
          Left  dups → fail $ toString dups
          Right hm'  → return $ HostMap hm'
  parseJSON invalid = typeMismatch "host map" invalid

hmHosts ∷ HostMap → [Host]
hmHosts (HostMap hm) = HashMap.elems hm

hostMapType ∷ Decoder HostMap
hostMapType = let hnKey h = (h ⊣ hname, h)
               in HostMap ∘ __fromList ∘ fmap hnKey ⊳ D.list hostType

instance FromDhall HostMap where
  autoWith _ = hostMapType

-- that's all, folks! ----------------------------------------------------------
