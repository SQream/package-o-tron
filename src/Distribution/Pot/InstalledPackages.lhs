

TODO:

parse the depends: field to get the complete set of package
dependencies

also want to know which packages come with ghc: maybe just hardcode
  the names for now

use a proper parser to parse all the information?
not sure if this information is available in Cabal or something



> module Distribution.Pot.InstalledPackages
>     (readPackages
>     ,lookupPackageForModule
>     ,PackageInfo(..)
>     ) where

> import Data.List
> import System.Process
> --import Text.Groom
> import Distribution.Pot.Types
> import Data.List.Split
> import Data.Maybe
> import Distribution.Package hiding (depends)
> --import Distribution.Simple.Configure
> import Distribution.InstalledPackageInfo
> --import Distribution.Verbosity
> --import Distribution.Simple
> --import Distribution.Simple.Program
> --import Distribution.Simple.PackageIndex
> --import Distribution.Simple.Configure
> --import Distribution.Simple.Setup
> import Distribution.ModuleName
> import qualified Data.Text as T

TODO: try and get this working

> {-readInstalledPackages :: IO [InstalledPackageInfo]
> readInstalledPackages = do
>   -- todo: get the package index properly -> instead of using D.S.GHC

configCompiler :: Maybe CompilerFlavor -> Maybe FilePath -> Maybe FilePath -> ProgramConfiguration -> Verbosity -> IO (Compiler, ProgramConfiguration)Source

>   --comp <- fst `fmap` configCompiler defaultCompilerFlavor Nothing Nothing
>   --                                  defaultProgramConfiguration normal
>   comp <- fst `fmap` configCompilerAux (defaultConfigFlags defaultProgramConfiguration)
>   pinf <- getInstalledPackages normal
>                                comp
>                                [GlobalPackageDB, UserPackageDB]
>                                defaultProgramConfiguration
>   return $ allPackages pinf-}


> lookupPackageForModule :: [PackageInfo] -> T.Text -> [T.Text]
> lookupPackageForModule pkgs m = do
>   map piName $ filter ((m `elem`) . piExposedModules) pkgs

> -- | parses the output of ghc-pkg dump
> readPackages :: IO [PackageInfo]
> readPackages = do
>   inf <- readProcess "ghc-pkg" ["dump"] ""
>   let pkgs = splitOn "---" inf
>       is = map parseInstalledPackageInfo pkgs
>   return $ mapMaybe (\i -> case i of
>                              ParseOk _ p -> Just $ pki p
>                              _ -> Nothing) is
>   where
>     pki p = PackageInfo (f $ installedPackageId p)
>                         (map mn $ exposedModules p)
>                         (map f $ depends p)
>     f (InstalledPackageId b) = -- strip hash and version
>        T.pack $ reverse $ dropWhile (`elem` "1234567890.-") $ dropWhile (/='-') $ reverse b
>     mn m = T.pack $ intercalate "." $ components m
