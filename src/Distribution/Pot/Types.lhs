
> module Distribution.Pot.Types where

> import Data.Text


> -- | represents the information parsed from a haskell source file
> data SourceSyntaxInfo =
>     SSI
>     {ssiModuleName :: Maybe Text
>     ,ssiImports :: [Text]}
>     deriving Show

> data ImportInfo = ImportFromPackage Text
>                 | ImportFromLocal Text
>                   deriving (Eq,Ord,Show)

> data AnnotatedSourceSyntaxInfo =
>     ASSI
>     {assiFilename :: FilePath -- ^ the path to the source file including the prefix folder
>     ,assiModuleName :: Maybe Text
>     ,assiImports :: [(Text,[ImportInfo])] -- ^ list of possible places the import can point to (so ambiguities with more than one source file and/or more than one package represented here)
>     }
>     deriving Show


> data PackageInf =
>     PackageInf
>     {piName :: String -- ^ the name of the package (hash/ version info stripped)
>     ,piExposedModules :: [String] -- ^ the names of the exposed modules, e.g. A.B.C
>     --,piDependencies :: [String] -- ^ the names of the packages that this package depends on
>     } deriving Show
