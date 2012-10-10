

TODO:

parse the depends: field to get the complete set of package
dependencies

also want to know which packages come with ghc: maybe just hardcode
  the names for now

use a proper parser to parse all the information?
not sure if this information is available in Cabal or something



> module Distribution.Pot.Packages (readPackages,lookupPackageForModule,PackageInf(..)) where

> import Data.List
> import System.Process
> --import Text.Groom
> import Distribution.Pot.Types


> --import Distribution.Simple.Configure
> --import Distribution.InstalledPackageInfo
> --import Distribution.Verbosity
> --import Distribution.Simple
> --import Distribution.Simple.Program
> --import Distribution.Simple.PackageIndex
> --import Distribution.Simple.Configure
> --import Distribution.Simple.Setup

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


> lookupPackageForModule :: [PackageInf] -> String -> [String]
> lookupPackageForModule pkgs m = do
>   map piName $ filter ((m `elem`) . piExposedModules) pkgs

> -- | returns a map from package name to the names of the modules in
> --   that package. The information is from the output of ghc-pkg dump, so
> --   only includes information from installed packages
> readPackages :: IO [PackageInf] -- (String,[String])]
> readPackages = do
>   inf <- readProcess "ghc-pkg" ["dump"] ""
>   return $ kludgePackages $ parsePackages [] $ lines inf
>   where
>     -- look for the "name:" for the start of the next module
>     parsePackages acc (f:v) | "name: " `isPrefixOf` f =
>         parseModules acc (drop (length "name: ") f) v
>     parsePackages acc (_:v) = parsePackages acc v
>     parsePackages acc [] = acc
>     -- look for exposed modules to get the list of modules in this
>     -- package
>     parseModules acc nm (f:v) | "exposed-modules:" `isPrefixOf` f =
>       parseMoreModules acc nm [drop (length "exposed-modules:") f] v
>     parseModules acc nm (_:v) = parseModules acc nm v
>     parseModules _ nm [] = error $ "didn't find exposed-modules for package " ++ nm
>     -- keep parsing the module list until we get a line without whitespace at the start
>     -- this works because of the way ghc-pkg pretty prints large fields
>     parseMoreModules acc nm macc ((' ':f):v) = parseMoreModules acc nm (f:macc) v
>     parseMoreModules acc nm macc v =
>       let ms = PackageInf nm (words $ unwords macc)
>       in parsePackages (ms:acc) v

> kludgePackages :: [PackageInf] -> [PackageInf]
>     -- lots of modules appear in base, haskell98, haskell2010
>     -- just use base for now.
> kludgePackages = filter ((`notElem` ["haskell98"
>                                     ,"haskell2010"]) . piName)
