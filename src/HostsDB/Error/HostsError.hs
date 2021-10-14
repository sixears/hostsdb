module HostsDB.Error.HostsError
  ( AsHostsError( _HostsError ), HostsError, HostsDomainError
  , HostsDomainExecCreateError
  , HostsDomainExecCreateIOError
  , HostsExecCreateError
  , HostsExecCreateIOError
  , aliasNotFound, danglingAlias, localnameNotFound
  )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq( (==) ) )
import Data.Function      ( ($), (&), id )
import GHC.Stack          ( CallStack, HasCallStack, callStack )
import Text.Show          ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- domainnames -------------------------

import DomainNames.Error.DomainError  ( AsDomainError( _DomainError )
                                      , DomainError )
import DomainNames.Hostname           ( Localname )

-- has-callstack -----------------------

import HasCallstack  ( HasCallstack( callstack ) )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError( _IOError ) )

-- lens --------------------------------

import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism', prism )
import Control.Lens.Review  ( (#) )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool    ( pattern 𝕱 )
import Data.MoreUnicode.Either  ( pattern 𝕷, pattern 𝕽 )
import Data.MoreUnicode.Lens    ( (⊣), (⊢), (⩼) )
import Data.MoreUnicode.Maybe   ( pattern 𝕵 )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- proclib -----------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError( _CreateProcError ) )
import ProcLib.Error.ExecError        ( AsExecError( _ExecError ) )
import ProcLib.Error.ExecCreateError  ( ExecCreateError, ExecCreateIOError
                                      , _ECCreateE, _ECExecE, _ECICreateE
                                      , _ECIExecE, _ECIIOE
                                      )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

data HostsError = LocalnameNotFoundError  Localname CallStack
                | DanglingAlias Localname Localname CallStack
  deriving Show

--------------------

instance Exception HostsError

--------------------

instance Eq HostsError where
  (LocalnameNotFoundError l1 _) == (LocalnameNotFoundError l2 _)  = l1 == l2
  (DanglingAlias       a1 l1 _) == (DanglingAlias       a2 l2 _)  =
    (a1,l1) == (a2,l2)
  _ == _ = 𝕱

--------------------

instance HasCallstack HostsError where
  callstack = lens (\ case LocalnameNotFoundError _ cs → cs
                           DanglingAlias        _ _ cs → cs)
                   (\ he cs → case he of LocalnameNotFoundError t _ →
                                           LocalnameNotFoundError t cs
                                         DanglingAlias a l _ →
                                           DanglingAlias a l cs
                   )

--------------------

instance Printable HostsError where
  print (LocalnameNotFoundError l _) = P.text $ [fmt|no such host '%T'|] l
  print (DanglingAlias a l _)        =
    P.text $ [fmt|alias %T points to non-existent '%T'|] a l

class AsHostsError δ where
  _HostsError ∷ Prism' δ HostsError

localnameNotFound ∷ ∀ ε α η .
                    (AsHostsError ε, MonadError ε η, HasCallStack) ⇒
                    Localname → η α
localnameNotFound l =
  throwError $ _HostsError # LocalnameNotFoundError l callStack

danglingAlias ∷ ∀ ε α η .
                (AsHostsError ε, MonadError ε η, HasCallStack) ⇒
                Localname → Localname → η α
danglingAlias a l = throwError $ _HostsError # DanglingAlias a l callStack

{- | convert LocalnameNotFoundErrors to DanglingAlias (from `a`); leave other
     HostsErrors alone
 -}
aliasNotFound ∷ (AsHostsError ε, HasCallStack) ⇒ Localname → ε → ε
aliasNotFound a e =
  case e ⩼ _HostsError  of
    𝕵 (LocalnameNotFoundError l cs) → _HostsError # DanglingAlias a l cs
    _                            → e

instance AsHostsError HostsError where
  _HostsError = id

------------------------------------------------------------

data HostsDomainError = HDHostsError  HostsError
                      | HDDomainError DomainError
  deriving (Eq, Show)

--------------------

instance Exception HostsDomainError

--------------------

instance HasCallstack HostsDomainError where
  callstack = lens (\ case HDHostsError  he → he ⊣ callstack
                           HDDomainError de → de ⊣ callstack)
                   (\ hde cs → case hde of HDHostsError  he →
                                             HDHostsError  $ he & callstack ⊢ cs
                                           HDDomainError de →
                                             HDDomainError $ de & callstack ⊢ cs
                   )

--------------------

_HDHostsError ∷ Prism' HostsDomainError HostsError
_HDHostsError = prism HDHostsError
                      (\ e → case e of HDHostsError e' → 𝕽 e'; _ → 𝕷 e)

_HDDomainError ∷ Prism' HostsDomainError DomainError
_HDDomainError = prism HDDomainError
                       (\ e → case e of HDDomainError e' → 𝕽 e'; _ → 𝕷 e)

instance AsHostsError HostsDomainError where
  _HostsError = _HDHostsError

instance AsDomainError HostsDomainError where
  _DomainError = _HDDomainError

------------------------------------------------------------

data HostsExecCreateError = HECExecCreateError ExecCreateError
                          | HECHostsError      HostsError
  deriving (Eq, Show)

--------------------

instance Exception HostsExecCreateError

--------------------

instance HasCallstack HostsExecCreateError where
  callstack = lens (\ case HECExecCreateError  ece → ece ⊣ callstack
                           HECHostsError       he  → he  ⊣ callstack)
                   (\ hdece cs →
                      case hdece of
                        HECExecCreateError ece →
                          HECExecCreateError $ ece & callstack ⊢ cs
                        HECHostsError he →
                          HECHostsError $ he & callstack ⊢ cs
                   )

--------------------

_HECExecCreateError ∷ Prism' HostsExecCreateError ExecCreateError
_HECExecCreateError =
  prism (\ e → HECExecCreateError e)
        (\ e → case e of HECExecCreateError e' → 𝕽 e'; _ → 𝕷 e)

_HECHostsError ∷ Prism' HostsExecCreateError HostsError
_HECHostsError = prism (\ e → HECHostsError e)
                       (\ e → case e of HECHostsError e' → 𝕽 e'; _ → 𝕷 e)

--------------------

instance AsHostsError HostsExecCreateError where
  _HostsError = _HECHostsError

--------------------

instance AsExecError HostsExecCreateError where
  _ExecError = _HECExecCreateError ∘ _ECExecE

--------------------

instance AsCreateProcError HostsExecCreateError where
  _CreateProcError = _HECExecCreateError ∘ _ECCreateE

------------------------------------------------------------

data HostsDomainExecCreateError = HDECExecCreateError  ExecCreateError
                                | HDECHostsDomainError HostsDomainError
  deriving (Eq, Show)

--------------------

instance Exception HostsDomainExecCreateError

--------------------

instance HasCallstack HostsDomainExecCreateError where
  callstack = lens (\ case HDECExecCreateError  ece → ece ⊣ callstack
                           HDECHostsDomainError hde → hde ⊣ callstack)
                   (\ hdece cs →
                      case hdece of
                        HDECExecCreateError ece →
                          HDECExecCreateError $ ece & callstack ⊢ cs
                        HDECHostsDomainError hde →
                          HDECHostsDomainError $ hde & callstack ⊢ cs
                   )

--------------------

_HDECExecCreateError ∷ Prism' HostsDomainExecCreateError ExecCreateError
_HDECExecCreateError =
  prism (\ e → HDECExecCreateError e)
        (\ e → case e of HDECExecCreateError e' → 𝕽 e'; _ → 𝕷 e)

--------------------

_HDECHostsDomainError ∷ Prism' HostsDomainExecCreateError HostsDomainError
_HDECHostsDomainError =
  prism (\ e → HDECHostsDomainError e)
        (\ e → case e of HDECHostsDomainError e' → 𝕽 e'; _ → 𝕷 e)

--------------------

instance AsHostsError HostsDomainExecCreateError where
  _HostsError = _HDECHostsDomainError ∘ _HDHostsError

--------------------

instance AsDomainError HostsDomainExecCreateError where
  _DomainError = _HDECHostsDomainError ∘ _HDDomainError

--------------------

instance AsExecError HostsDomainExecCreateError where
  _ExecError = _HDECExecCreateError ∘ _ECExecE

--------------------

instance AsCreateProcError HostsDomainExecCreateError where
  _CreateProcError = _HDECExecCreateError ∘ _ECCreateE

------------------------------------------------------------

data HostsExecCreateIOError = HECIExecCreateIOError ExecCreateIOError
                            | HECIHostsError        HostsError
  deriving (Eq, Show)

--------------------

instance Exception HostsExecCreateIOError

--------------------

instance HasCallstack HostsExecCreateIOError where
  callstack = lens (\ case HECIExecCreateIOError ecioe → ecioe ⊣ callstack
                           HECIHostsError        he    → he    ⊣ callstack)
                   (\ hecioe cs →
                      case hecioe of
                        HECIExecCreateIOError ecioe →
                          HECIExecCreateIOError $ ecioe & callstack ⊢ cs
                        HECIHostsError he →
                          HECIHostsError        $ he    & callstack ⊢ cs
                   )

--------------------

_HECIExecCreateIOError ∷ Prism' HostsExecCreateIOError ExecCreateIOError
_HECIExecCreateIOError =
  prism (\ e → HECIExecCreateIOError e)
        (\ e → case e of HECIExecCreateIOError e' → 𝕽 e'; _ → 𝕷 e)

--------------------

_HECIHostsError ∷ Prism' HostsExecCreateIOError HostsError
_HECIHostsError =
  prism (\ e → HECIHostsError e)
        (\ e → case e of HECIHostsError e' → 𝕽 e'; _ → 𝕷 e)

--------------------

instance AsHostsError HostsExecCreateIOError where
  _HostsError = _HECIHostsError

--------------------

instance AsExecError HostsExecCreateIOError where
  _ExecError = _HECIExecCreateIOError ∘ _ECIExecE

--------------------

instance AsCreateProcError HostsExecCreateIOError where
  _CreateProcError = _HECIExecCreateIOError ∘ _ECICreateE

--------------------

instance AsIOError HostsExecCreateIOError where
  _IOError = _HECIExecCreateIOError ∘ _ECIIOE

------------------------------------------------------------

data HostsDomainExecCreateIOError = HDECIExecCreateIOError ExecCreateIOError
                                  | HDECIHostsDomainError  HostsDomainError
  deriving (Eq, Show)

--------------------

instance Exception HostsDomainExecCreateIOError

--------------------

instance HasCallstack HostsDomainExecCreateIOError where
  callstack = lens (\ case HDECIExecCreateIOError ecioe → ecioe ⊣ callstack
                           HDECIHostsDomainError  hde   → hde   ⊣ callstack)
                   (\ he cs → case he of
                               HDECIExecCreateIOError ecioe →
                                 HDECIExecCreateIOError $ ecioe & callstack ⊢ cs
                               HDECIHostsDomainError hde →
                                 HDECIHostsDomainError $ hde & callstack ⊢ cs
                   )

--------------------

_HDECIExecCreateIOError ∷ Prism' HostsDomainExecCreateIOError ExecCreateIOError
_HDECIExecCreateIOError =
  prism (\ e → HDECIExecCreateIOError e)
        (\ e → case e of HDECIExecCreateIOError e' → 𝕽 e'; _ → 𝕷 e)

--------------------

_HDECIHostsDomainError ∷ Prism' HostsDomainExecCreateIOError HostsDomainError
_HDECIHostsDomainError =
  prism (\ e → HDECIHostsDomainError e)
        (\ e → case e of HDECIHostsDomainError e' → 𝕽 e'; _ → 𝕷 e)

--------------------

instance AsHostsError HostsDomainExecCreateIOError where
  _HostsError = _HDECIHostsDomainError ∘ _HDHostsError

--------------------

instance AsDomainError HostsDomainExecCreateIOError where
  _DomainError = _HDECIHostsDomainError ∘ _HDDomainError

--------------------

instance AsExecError HostsDomainExecCreateIOError where
  _ExecError = _HDECIExecCreateIOError ∘ _ECIExecE

--------------------

instance AsCreateProcError HostsDomainExecCreateIOError where
  _CreateProcError = _HDECIExecCreateIOError ∘ _ECICreateE

--------------------

instance AsIOError HostsDomainExecCreateIOError where
  _IOError = _HDECIExecCreateIOError ∘ _ECIIOE

-- that's all, folks! ----------------------------------------------------------
