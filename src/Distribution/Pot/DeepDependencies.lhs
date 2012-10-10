

> {-# LANGUAGE TupleSections #-}
> module Distribution.Pot.DeepDependencies where

> import Distribution.Pot.Types
> --import Distribution.Pot.HaskellSourceParser

> --import qualified Data.Text as T
> --import qualified Data.Text.Lazy.IO as LT
> --import System.FilePath
> --import Control.Monad
> import Data.List
> --import Distribution.Pot.InstalledPackages
> --import Control.Monad.State
> --import Control.Applicative
> import Data.Maybe
> --import System.Directory
> import Data.Either
> --import Control.Arrow

creates the deep dependencies for each entry
todo: use state monad to avoid repeated tree traversals

> deepDependencies :: [PackageInfo] -> [AnnotatedSSI] -> [DeepSSI]
> deepDependencies pkgs assis =
>   let transitiveImports assi =
>         let imports = assiImports assi
>             depFns = lefts $ concatMap (map splitDep . snd) imports
>             depssis = filter ((`elem` depFns) . assiFilename) assis
>         in imports ++ concatMap transitiveImports depssis
>       packMap = map (\p -> (piName p, piDependencies p)) pkgs
>       allPackDeps p = let ds = fromMaybe [] $ lookup p packMap
>                       in p : concatMap allPackDeps ds
>   in flip map assis $ \assi ->
>          let deepImps = sort $ nub $ transitiveImports assi
>              deepPks = rights $ concatMap (map splitDep . snd) deepImps
>          in DSSI {dssiAssi = assi
>                  ,dssiDeepImports = deepImps
>                  ,dssiDeepDeepPackages = sort $ nub $ concatMap allPackDeps $ deepPks}
