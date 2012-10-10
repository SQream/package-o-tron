
> module Distribution.Pot.Types where

> import qualified Data.Text as T
> import Data.Either
> import Data.List
> import Control.Arrow

> -- | represents the information parsed from a haskell source file
> data SourceSyntaxInfo =
>     SSI
>     {ssiModuleName :: Maybe T.Text
>     ,ssiImports :: [T.Text]
>     } deriving (Eq,Show)

> data ImportInfo = ImportFromPackage T.Text
>                 | ImportFromLocal T.Text
>                   deriving (Eq,Ord,Show)

> data AnnotatedSSI =
>     ASSI
>     {assiFilename :: FilePath -- ^ the path to the source file including the prefix folder
>     ,assiModuleName :: Maybe T.Text
>     ,assiImports :: [(T.Text,[ImportInfo])] -- ^ list of possible places the import can point to (so ambiguities with more than one source file and/or more than one package represented here)
>     } deriving (Eq,Show)

> data DeepSSI =
>     DSSI
>     {dssiAssi :: AnnotatedSSI
>     ,dssiDeepImports :: [(T.Text,[ImportInfo])] -- ^ all the import infos dependencies transitively
>     ,dssiDeepDeepPackages :: [T.Text] -- ^ all the packages referenced by this and local dependencies, plus all the transitively dependent packages for those packages (i.e. all the packages needed on the system needed for this module)
>     } deriving (Eq,Show)

todo change to text?

> data PackageInfo =
>     PackageInfo
>     {piName :: T.Text -- ^ the name of the package (hash/ version info stripped)
>     ,piExposedModules :: [T.Text] -- ^ the names of the exposed modules, e.g. A.B.C
>     ,piDependencies :: [T.Text] -- ^ the names of the packages that this package depends on
>     } deriving (Eq,Show)

> splitDep :: ImportInfo -> Either FilePath T.Text
> splitDep (ImportFromLocal x) = Left $ T.unpack x
> splitDep (ImportFromPackage y) = Right y

TODO: change to text?

> ppEssi :: DeepSSI -> String
> ppEssi dssi =
>   let assi = dssiAssi dssi
>       sn :: (Ord a, Eq a) => [a] -> [a]
>       sn = sort . nub
>       tsn :: (Ord a, Eq a, Ord b, Eq b) => ([a],[b]) -> ([a],[b])
>       tsn = sn *** sn
>       (ids,ips) = tsn $ partitionEithers $ concatMap (map splitDep . snd) $ assiImports assi
>       (dds, dps) = tsn $ partitionEithers $ concatMap (map splitDep . snd) $ dssiDeepImports dssi
>   in assiFilename assi
>      ++ ": " ++ show (assiModuleName assi)
>      ++ "\n----------\n"
>      ++ unlines ("deps" : ids)
>      ++ "\n" ++ unlines ("packs" : map T.unpack ips)
>      ++ "\n" ++ unlines ("deepdeps" : dds)
>      ++ "\n" ++ unlines ("deeppacks" : map T.unpack dps)
>      ++ "\n" ++ unlines ("allpacks" : map T.unpack (dssiDeepDeepPackages dssi))
