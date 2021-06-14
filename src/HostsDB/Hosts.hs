module HostsDB.Hosts
  ( Domains( Domains ), HasHosts( hosts ), Hosts( Hosts )
  , aliases, aliasHosts, dnsServers, domains, hostsHosts, hostIPs
  , hostIPv4, hostIPv4', hostIPv4s, inAddr, lhostmap, loadFile, loadFile'
  , lookupHost, lookupHost', mailServers, subDomain
  )
where

-- aeson -------------------------------

import Data.Aeson.Types  ( FromJSON )

-- base --------------------------------

import Control.Monad           ( return )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Eq                 ( Eq )
import Data.Function           ( ($), id )
import Data.Functor            ( fmap )
import Data.List               ( intercalate )
import Data.Maybe              ( maybe )
import Data.Tuple              ( swap )
import GHC.Generics            ( Generic )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- containers --------------------------

import qualified  Data.Map  as  Map

-- containers-plus ---------------------

import ContainersPlus.MapUtils  ( fromListWithDups )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- dhall -------------------------------

import qualified  Dhall  as  D
import Dhall  ( FromDhall( autoWith ), auto, field, record )

-- dhall-plus --------------------------

import DhallPlus        ( parseFile )
import DhallPlus.Error  ( AsDhallError, DhallIOError )

-- domainnames -------------------------

import DomainNames.FQDN      ( FQDN )
import DomainNames.Hostname  ( Hostname, Localname, filterWL )

-- fpath -------------------------------

import FPath.AsFilePath  ( AsFilePath )
import FPath.File        ( FileAs )

-- ip4 ---------------------------------

import IP4  ( IP4 )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Lens    ( Lens', lens )

-- monaderror-io -----------------------

import MonadError           ( mapMError )
import MonadError.IO.Error  ( AsIOError )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.Text         ( 𝕋 )

-- non-empty-containers ----------------

import NonEmptyContainers.NonEmptyHashSet  ( NonEmptyHashSet )

-- unordered-containers ----------------

import Data.HashMap.Strict  ( HashMap, lookup, traverseWithKey )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import HostsDB.Error.HostsError  ( AsHostsError, HostsError
                                 , aliasNotFound, localnameNotFound )
import HostsDB.Host              ( Host, hname, ipv4 )
import HostsDB.LHostMap          ( LHostMap, lhmHosts, unLHostMap )
import HostsDB.LocalnameMap      ( LocalnameMap, unLHMap )

--------------------------------------------------------------------------------

class HasSubDomain α where
  subDomain ∷ Lens' α FQDN

class HasINAddr α where
  inAddr ∷ Lens' α FQDN

------------------------------------------------------------

data Domains = Domains { _subDomain ∷ FQDN, _inAddr ∷ FQDN }
  deriving (Eq, FromJSON, Generic, NFData, Show)

instance HasSubDomain Domains where
  subDomain = lens _subDomain (\ d s → d { _subDomain = s })

instance HasINAddr Domains where
  inAddr = lens _inAddr (\ d i → d { _inAddr = i })

instance FromDhall Domains where
  autoWith _ = record $ Domains ⊳ field "sub_domain" auto
                                ⊵ field "in_addr" auto

------------------------------------------------------------

data Hosts = Hosts { _domains      ∷ Domains
                   , _lhostmap     ∷ LHostMap
                   , _dnsServers   ∷ [Localname]
                   , _mailServers  ∷ [Localname]
                   , _aliases      ∷ LocalnameMap
                   }
  deriving (Eq, FromJSON, Generic, NFData)

class HasHosts α where
  hosts ∷ Lens' α Hosts

instance HasHosts Hosts where
  hosts = id

instance HasSubDomain Hosts where
  subDomain = domains ∘ subDomain

instance HasINAddr Hosts where
  inAddr = domains ∘ inAddr

instance FromDhall Hosts where
  autoWith _ = record $ Hosts ⊳ field "domains"      auto
                              ⊵ field "hosts"        auto
                              ⊵ field "dns_servers"  (D.list auto)
                              ⊵ field "mail_servers" (D.list auto)
                              ⊵ field "aliases"      auto

instance Show Hosts where
  show h = intercalate "\n" [ "HOSTS:       " ⊕ show (h ⊣ lhostmap)
                            , "DNSSERVERS:  " ⊕ show (h ⊣ dnsServers)
                            , "MAILSERVERS: " ⊕ show (h ⊣ mailServers)
                            , "ALIASES:     " ⊕ show (h ⊣ aliases)
                            ]

----------------------------------------

domains      ∷ Lens' Hosts Domains
domains      = lens _domains (\ hs d → hs { _domains = d })

lhostmap     ∷ Lens' Hosts LHostMap
lhostmap     = lens _lhostmap (\ hs lhm → hs { _lhostmap = lhm })

dnsServers  ∷ Lens' Hosts [Localname]
dnsServers  = lens _dnsServers (\ hs ds → hs { _dnsServers = ds })

mailServers ∷ Lens' Hosts [Localname]
mailServers = lens _mailServers (\ hs ms → hs { _mailServers = ms })

aliases       ∷ Lens' Hosts LocalnameMap
aliases       = lens _aliases (\ hs as → hs { _aliases = as })

----------------------------------------

lookupHost ∷ (AsHostsError ε, MonadError ε η) ⇒ Hosts → Localname → η Host
lookupHost hs l = let hs' = unLHostMap $ view lhostmap hs
                   in maybe (localnameNotFound l) return $ lookup l hs'

--------------------

lookupHost' ∷ MonadError HostsError η ⇒ Hosts → Localname → η Host
lookupHost' = lookupHost

----------------------------------------

hostIPv4 ∷ (AsHostsError ε, MonadError ε η) ⇒ Hosts → Localname → η IP4
hostIPv4 hs h = view ipv4 ⊳ lookupHost hs h

--------------------

hostIPv4' ∷ MonadError HostsError η ⇒ Hosts → Localname → η IP4
hostIPv4' = hostIPv4

----------------------------------------

hostsHosts ∷ Hosts → [Host]
hostsHosts = lhmHosts ∘ view lhostmap

----------------------------------------

hostIPv4s ∷ Hosts → [(Hostname,IP4)]
hostIPv4s = fmap ( \ h → (h ⊣ hname, h ⊣ ipv4) ) ∘ hostsHosts

----------------------------------------

{- | a map from local alias names to the underlying host (if any) -}
aliasHosts ∷ (AsHostsError ε, MonadError ε η) ⇒
             Hosts → η (HashMap Localname Host)
aliasHosts hs =
  traverseWithKey (\ l a → mapMError (aliasNotFound l) $ lookupHost hs a)
                  (unLHMap (view aliases hs))

----------------------------------------

{- | Find all the "valid" Hostname → IP4 mappings, ignoring hosts called α-wl
     that share an IP with α; and additionally return errors for (other)
     duplicates and missing IPs, etc.
 -}
hostIPs ∷ Hosts → ([(Hostname,IP4)],[𝕋])
hostIPs hs =
  let dupIPHosts ∷ Map.Map IP4 (NonEmptyHashSet Hostname)
      hostsByIP  ∷ Map.Map IP4 Hostname
      (dupIPHosts, hostsByIP) = fromListWithDups $ swap ⊳ hostIPv4s hs

      (filteredDups,es) = filterWL dupIPHosts
      hostList = swap ⊳ ю [ Map.toList hostsByIP
                          , Map.toList filteredDups ]
   in (hostList, es)

----------------------------------------

loadFile ∷ ∀ ε γ μ .
           (MonadIO μ, AsDhallError ε, AsIOError ε, MonadError ε μ,
            FileAs γ, AsFilePath γ) ⇒
           γ -> μ Hosts

loadFile = parseFile -- @_ @_ @_ @Hosts

--------------------

{-# DEPRECATED loadFile' "use loadFile @DhallIOError" #-}
loadFile' ∷ forall γ μ .
            (MonadIO μ, MonadError DhallIOError μ, FileAs γ, AsFilePath γ) ⇒
            γ -> μ Hosts
loadFile' = loadFile

-- that's all, folks! ----------------------------------------------------------
