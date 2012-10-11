

> {-# LANGUAGE TupleSections #-}
> module Distribution.Pot.DeepDependencies where

> import Distribution.Pot.Types
> import Data.List
> import Data.Maybe
> import Data.Either

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
>                   -- these are all the direct package dependencies of
>                   -- the file and all its local dependency sources
>                   -- the list of packages to pass to ghc when linking
>                  ,dssiDeepImports = deepImps
>                   -- these are all the above packages plus all their dependencies
>                   -- take this list, remove the packages that come with ghc
>                   -- now you have exactly all the packages you need to download
>                   -- on a fresh sandbox/system to compile the source code
>                  ,dssiDeepDeepPackages = sort $ nub $ concatMap allPackDeps $ deepPks}
