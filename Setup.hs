{- Custom Setup: generate the Futhark C backend (src/fut/Calculation.c, the
   c-sources entry in kid.cabal) before Cabal builds the library, so a plain
   `cabal build` no longer needs a separate `make` step in src/fut.

   The Futhark package dependencies (src/fut/lib/...) must still be fetched once
   beforehand with `make pkgs` (or are provided by the Nix build). -}

import           Control.Monad           (when)
import           Distribution.Simple
import           Distribution.Simple.Setup (BuildFlags, ConfigFlags)
import           Distribution.Types.HookedBuildInfo (HookedBuildInfo)
import           System.Directory        (doesFileExist, getModificationTime, withCurrentDirectory)
import           System.Process          (callProcess)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preConf  = \args flags -> genFuthark >> preConf  simpleUserHooks args flags
  , preBuild = \args flags -> genFuthark >> preBuild simpleUserHooks args flags
  }

-- | Run `futhark c --library Calculation.fut` in src/fut, but only when the
-- generated C is missing or older than the Futhark source, to avoid needless
-- recompilation on every build.
genFuthark :: IO ()
genFuthark = withCurrentDirectory "src/fut" $ do
  let src = "Calculation.fut"
      out = "Calculation.c"
  stale <- do
    exists <- doesFileExist out
    if not exists
      then return True
      else do
        srcT <- getModificationTime src
        outT <- getModificationTime out
        return (srcT > outT)
  when stale $ callProcess "futhark" ["c", "--library", src]
