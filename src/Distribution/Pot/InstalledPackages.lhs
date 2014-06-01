
> module Distribution.Pot.InstalledPackages
>     (readPackages
>     ,lookupPackageForModule
>     ) where

> import Data.List
> import System.Process
> import Distribution.Pot.Types
> import Data.List.Split
> import Data.Maybe
> import Distribution.Package hiding (depends)
> import Distribution.InstalledPackageInfo
> import Distribution.ModuleName
> import qualified Data.Text as T

> -- | finds all the packages which contain a given module
> lookupPackageForModule :: [PackageInfo] -- ^ the return value of readPackages
>                        -> T.Text -- ^ the module name to match (e.g. A.B.C)
>                        -> [T.Text] -- ^ the matching package names
> lookupPackageForModule pkgs m =
>   map piName $ filter ((m `elem`) . piExposedModules) pkgs

> -- | parses the output of ghc-pkg dump
> -- the filepath is to set a different package db from user
> readPackages :: Maybe FilePath -> IO [PackageInfo]
> readPackages fp = do
>   let as = case fp of
>                Just fp' -> ["--global","--package-db",fp']
>                Nothing -> []
>   inf <- readProcess "ghc-pkg" ("dump":as) ""
>   let pkgs = splitOn "---" inf
>       is = map parseInstalledPackageInfo pkgs
>   return $ mapMaybe (\i -> case i of
>                              ParseOk _ p -> Just $ pki p
>                              _ -> Nothing) is
>   where
>     pki p = PackageInfo (f $ installedPackageId p)
>                         (map mn $ exposedModules p)
>                         (filter (not . T.null) $ map f $ depends p)
>     f (InstalledPackageId b) = -- strip hash and version
>        T.pack $ reverse $ dropSuf $ dropSuf $ reverse b
>     mn m = T.pack $ intercalate "." $ components m
>     dropSuf = drop 1 . dropWhile (/='-')
