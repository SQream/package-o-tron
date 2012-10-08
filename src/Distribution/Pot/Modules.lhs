
simplistic parser which can list the modules imported by a haskell
source file

provides a recursive-memoized mode which can efficiently get the
complete transitive imports of a set of files

provides a method of splitting the imports into modules which are in
the local fileset, and modules which aren't (the local module
dependencies can be used for a build tool, and the non-local ones for
determining which packages are referenced)

> {-# LANGUAGE TupleSections,ScopedTypeVariables #-}
> module Distribution.Pot.Modules
>     (ModuleInfo(..)
>     ,modulesInfo
>     ,filterModules
>     ,ppMI) where

> import Data.Maybe
> import qualified System.FilePath.Find as F
> import System.FilePath.Find ((==?),(||?))
> import System.FilePath
> --import Text.Groom
> import Data.List
> import Distribution.Pot.Packages
> import Control.Arrow
> import Data.Either
> import Control.Applicative
> --import Debug.Trace


Rough code to parse out all the imports from a haskell source. Not
robust but I think should work on normal haskell source files.

> parseImports :: String -> [String]
> parseImports src = nub $ mapMaybe (im . words) $ lines src
>   where
>     im (">":x:_) | x /= "import" = Nothing
>     im (">":"import":"qualified":nm:_) = Just nm
>     im (">":"import":nm:_) = Just nm
>     im (x:_) | x /= "import" = Nothing
>     im ("import":"qualified":nm:_) = Just nm
>     im ("import":nm:_) = Just nm
>     im _ = Nothing

reads the folders passed in as source roots and returns a list of all
the modules (returning the root prefix folder (which is a elem of the
dirs argument, and the path to the module relative to its root prefix)

> getLocalModules :: [FilePath] -> IO [(FilePath,FilePath)]
> getLocalModules dirs =
>   concat <$> mapM findHs dirs
>   where
>     findHs dir =
>        map ((dir,) . makeRelative dir) <$>
>        F.find (return True) (F.extension ==? ".lhs" ||? F.extension ==? ".hs") dir

todo: don't store the FilePath,FilePath -> can get this back
use proper data type for a module name

> -- | Represents the collected information on one module
> data ModuleInfo =
>     ModuleInfo
>     -- todo: just the module name and the full filepath
>     {miFilename :: FilePath -- ^ the filename of the module relative to its root (e.g. A.B.C -> A/B/C.hs)
>     ,miFilePrefix :: FilePath -- ^ the root for the filename e.g. src/
>     ,miModuleFile :: FilePath -- ^ the complete filename including root e.g. src/A/B/C.hs
>     ,miModuleName :: String -- ^ the name of the module e.g A.B.C
>     ,miPackageImports :: [String] -- ^ the list of imported module names which come from packages
>     -- todo: just the full filepath, modulename
>     ,miLocalDeps :: [((FilePath,FilePath),String)] -- ^ the prefix, filename and module name for all the imports in the module which reference other files in the source
>     ,miLocalDeepDeps :: [((FilePath,FilePath),String)] -- ^ the same info for all the transitive dependencies in all the dependencies in the source
>     ,miDirectPackages :: [String] -- ^ the package names for all the imports in the module source which don't refer to local files
>     ,miDirectDeepPackages :: [String] -- ^ the transitive packages for this module and all its 'local deep dependencies'
>     ,miAllDeepPackages :: [String] -- ^ the direct deep packages plus all the transitive package dependencies for these packages, so the direct deep packages is all the packages you need as dependencies e.g. when calling ghc or in a .cabal, and the all deep packages is all the packages you need installed for this to work
>     } deriving Show

> ppMI :: ModuleInfo -> String
> ppMI mi =
>     unlines $ [miModuleName mi ++ " " ++ miModuleFile mi
>               ,"--------------"]
>                ++ ["", "= package imports"] ++ miPackageImports mi
>                ++ ["", "= direct"] ++ map snd (miLocalDeps mi)
>                ++ ["", "= deep"] ++ map snd (miLocalDeepDeps mi)
>                ++ ["", "= packs"] ++ miDirectPackages mi
>                ++ ["", "= deep"] ++ miDirectDeepPackages mi

> -- | Takes a set of source files and gets the ModuleInfo information
> --   for all of them
> modulesInfo :: [FilePath] -- ^ the root folders containing the source files
>                           -- to analyze

TODO: pass root folders + file list
or parse the files to determine the root folders
refactor this code so it is more straightforward

>             -> IO [ModuleInfo]
> modulesInfo srcs = do

get the module infos populated with just the module name, the all the
imports in the localdependencies field

>     lms <- getLocalModules srcs
>     (modsNoFileInfo :: [ModuleInfo]) <- mapM (uncurry moduleLocalDeps) lms
>     -- create the map from local module name to filename
>     let localModuleFilenames :: [(String, (FilePath, FilePath))]
>         localModuleFilenames =
>             flip map modsNoFileInfo $ \m ->
>                 (miModuleName m,(miFilePrefix m, miFilename m))

process the miLocalDependencies and for each import add the filename
info if it is from a local file or remove it from the list and add the
package name if it is from another package

>     pkgs <- readPackages
>     let modsDirectDeps = map (fixDirectDeps localModuleFilenames pkgs) modsNoFileInfo

populate the deeps

>         localDepMap :: [(String,((FilePath,FilePath),String))]
>         localDepMap = flip concatMap modsDirectDeps $ \mi ->
>             map (miModuleName mi,) $ miLocalDeps mi
>         modsLDDeps = map (fixLocalDeepDeps localDepMap) modsDirectDeps
>         packImportMap :: [(String,[String])]
>         packImportMap = map (\mi -> (miModuleName mi, miPackageImports mi)) modsDirectDeps
>         -- pass it the pkg info to lookup a package for a module
>         -- and the map from module name to names of imported local modules
>         -- and the map from module name to names of import package modules
>         modsDPDeps = flip map modsLDDeps $ \mi ->
>             mi {miDirectDeepPackages =
>                     sort $ nub
>                     $ deepPacks pkgs (map (second snd) localDepMap)
>                                 packImportMap $ miModuleName mi}
>     return modsDPDeps
>   where
>     fileNameToModuleName = map (\c -> case c of
>                                              '/' -> '.'
>                                              _ -> c)
>                            . dropExtension
>     moduleLocalDeps pre fn = do
>       is <- parseImports <$> readFile (pre </> fn)
>       -- return a partially filled in ModuleInfo structure
>       return $ ModuleInfo {miFilename = fn
>                           ,miFilePrefix = pre
>                           ,miModuleFile = pre </> fn
>                           ,miModuleName = fileNameToModuleName fn
>                           ,miPackageImports = []
>                           -- fill in the filename info as blank for now
>                           ,miLocalDeps = zip (repeat ("","")) $ nub is
>                           -- all the other fields filled in as blank for now
>                           ,miLocalDeepDeps = []
>                           ,miDirectPackages = []
>                           ,miDirectDeepPackages = []
>                           ,miAllDeepPackages = []}

take the mi with all the imports in the localdeps without filenames
and fix this so all the localdeps are real localdeps and have the
filename information
and the direct package information is filled in from the non localdeps

>     fixDirectDeps :: [(String,(FilePath,FilePath))] -> [PackageInf] -> ModuleInfo -> ModuleInfo
>     fixDirectDeps localModuleFilenames pkgs mi =
>         let (localDeps,packageDeps) = partitionEithers
>                                       $ map (lookupModule localModuleFilenames pkgs . snd)
>                                       $ miLocalDeps mi
>         in mi {miLocalDeps = nub localDeps
>               ,miDirectPackages = sort $ nub $ concat packageDeps
>               ,miPackageImports = sort $ nub (map snd $ miLocalDeps mi) \\ nub (map snd $ localDeps)}
>     lookupModule localModuleFilenames pkgs mn =
>         case () of
>             _ | Just finf <- lookup mn localModuleFilenames -> Left (finf,mn)
>               | otherwise ->
>                     case lookupPackageForModule pkgs mn of
>                         [] -> error $ "module not found: " ++ mn

if there is more than one package match, just add all the matching
packages, this could be wrong at the moment I've seen this with
packages in base, haskell98 and/or haskell2010 the other code deals
with this by only using base

>                         tpkgs -> Right tpkgs

populate the local deep deps. This is possibly a bit slow since it
traverses repeatedly through the map for each module. TODO: do this
without repeated traversals (?create a map with state monad
recursively, then fill in the fields)

>     fixLocalDeepDeps :: [(String,((FilePath,FilePath),String))]
>                      -> ModuleInfo -> ModuleInfo
>     fixLocalDeepDeps ldm mi =
>       mi {miLocalDeepDeps = filter (/=((miFilePrefix mi,miFilename mi),miModuleName mi))
>                             $ sort $ nub $ concatMap (deepDeps ldm) (miLocalDeps mi)}
>     deepDeps :: [(String,((FilePath,FilePath),String))]
>              -> ((FilePath,FilePath),String)
>              -> [((FilePath,FilePath),String)]
>     deepDeps ldm mfn =
>       let dmfns = map snd $ filter ((== snd mfn) . fst) ldm
>       in mfn : concatMap (deepDeps ldm) dmfns
>
>     deepPacks :: [PackageInf] -> [(String,String)] -> [(String,[String])] -> String -> [String]
>     deepPacks pkgs limps pimps mn =
>         let lis :: [String]
>             lis = map snd $ filter ((==mn) . fst) limps
>             pis :: [String]
>             pis = concatMap snd $ filter ((==mn) . fst) pimps
>             lps :: [String]
>             lps = concatMap (lookupPackageForModule pkgs) pis
>             subPacks = concatMap (deepPacks pkgs limps pimps) lis
>         in lps ++ subPacks

> -- | helper to filter the two module lists according to some function
> filterModules :: (String -> Bool) -> ModuleInfo -> ModuleInfo
> filterModules f mi = mi {miDirectPackages = filter f $ miDirectPackages mi
>                         ,miDirectDeepPackages = filter f $ miDirectDeepPackages mi
>                         ,miAllDeepPackages = filter f $ miAllDeepPackages mi}
