
> module Distribution.Pot.Types where

> import qualified Data.Text as T
> import Data.Either
> import Data.List
> import Control.Arrow
> import Data.Maybe

> -- | represents the information parsed from a haskell source file
> data SourceImports =
>     SourceImports
>     {siModuleName :: Maybe T.Text
>     ,siImports :: [T.Text]
>     } deriving (Eq,Show)

> -- | an ImportInfo represents a matching reference for an import declaration.
> -- Either a package name, or a FilePath to the local source
> data ImportInfo = ImportFromPackage T.Text
>                 | ImportFromLocal FilePath
>                   deriving (Eq,Ord,Show)

> -- | a partially parsed source file with the imports labeled with matching source files and/or packages
> data SourceDeps =
>     SourceDeps
>     {sdFilename :: FilePath -- ^ the path to the source file including the prefix folder
>     ,sdModuleName :: Maybe T.Text -- ^ module name in the source, Nothing if there is no module declaration
>     ,sdImports :: [(T.Text,[ImportInfo])] -- ^ list of possible places the import can point to (so ambiguities with more than one source file and/or more than one package represented here)
>     } deriving (Eq,Show)

> -- | get the package dependencies from the imports
> packageDeps :: SourceDeps -> [T.Text]
> packageDeps = mapMaybe (\x -> case x of
>                                 ImportFromPackage p -> Just p
>                                 _ -> Nothing)
>               . concatMap snd
>               . sdImports

> -- | represents the deep dependencies for a source file
> data DeepSourceDeps =
>     DeepSourceDeps
>     {ddSd :: SourceDeps
>     ,ddDeepImports :: [(T.Text,[ImportInfo])] -- ^ all the import infos dependencies transitively
>     ,ddDeepDeepPackages :: [T.Text] -- ^ all the packages referenced by this and local dependencies, plus all the transitively dependent packages for those packages (i.e. all the packages needed on the system needed for this module)
>     } deriving (Eq,Show)


> -- | information on an installed package
> data PackageInfo =
>     PackageInfo
>     {piName :: T.Text -- ^ the name of the package (hash/ version info stripped)
>     ,piExposedModules :: [T.Text] -- ^ the names of the exposed modules, e.g. A.B.C
>     ,piDependencies :: [T.Text] -- ^ the names of the packages that this package depends on
>     } deriving (Eq,Show)

> -- | helper for working with ImportInfo
> splitDep :: ImportInfo -> Either FilePath T.Text
> splitDep (ImportFromLocal x) = Left x
> splitDep (ImportFromPackage y) = Right y

TODO: change to text?

> -- | slighltly nicely formatted display of a DeepSSI
> ppEssi :: DeepSourceDeps -> String
> ppEssi dssi =
>   let assi = ddSd dssi
>       sn :: (Ord a, Eq a) => [a] -> [a]
>       sn = sort . nub
>       tsn :: (Ord a, Eq a, Ord b, Eq b) => ([a],[b]) -> ([a],[b])
>       tsn = sn *** sn
>       (ids,ips) = tsn $ partitionEithers $ concatMap (map splitDep . snd) $ sdImports assi
>       (dds, dps) = tsn $ partitionEithers $ concatMap (map splitDep . snd) $ ddDeepImports dssi
>   in sdFilename assi
>      ++ ": " ++ show (sdModuleName assi)
>      ++ "\n----------\n"
>      ++ unlines ("deps" : ids)
>      ++ "\n" ++ unlines ("packs" : map T.unpack ips)
>      ++ "\n" ++ unlines ("deepdeps" : dds)
>      ++ "\n" ++ unlines ("deeppacks" : map T.unpack dps)
>      ++ "\n" ++ unlines ("allpacks" : map T.unpack (ddDeepDeepPackages dssi))
