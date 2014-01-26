module Import
    ( module Import
    ) where

import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import           Yesod                as Import hiding (Route)

import           Control.Applicative  as Import (pure, (<$>), (<*>))
import           Data.Text            as Import (Text, unpack)

import           Foundation           as Import
import           Model                as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

-- | Returns the approot string. Very ugly stuff.
approotString :: App -> String
approotString master =
  case approot of
    ApprootStatic textRoot -> unpack textRoot
    ApprootMaster f -> unpack $ f master
    _ -> error "Could not get the approot string! This was a hack anyway, pester the developer."

relativeToAbsolute :: App -> String -> String
relativeToAbsolute master relative = approotString master ++ "/" ++ relative