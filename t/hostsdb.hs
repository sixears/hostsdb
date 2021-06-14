{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UnicodeSyntax         #-}

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( failureCode, fullDesc, info, prefs
                                    , progDesc, showHelpOnError )
import Options.Applicative.Extra    ( customExecParser, helper )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-plus --------------------------

import TastyPlus  ( runTests_, tastyOptParser )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  HostsDB.T.Host
import qualified  HostsDB.T.Hosts

--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "tinydns" [ HostsDB.T.Host.tests
                            , HostsDB.T.Hosts.tests
                            ]

main ∷ IO ()
main = do
  tastyOpts ← customExecParser (prefs showHelpOnError) $
                info (helper ⊵ tastyOptParser tests)
                     (fullDesc ⊕ progDesc "tests for tinydns package"
                               ⊕ failureCode 254)

  _ ← runTests_ tastyOpts
  return ()
