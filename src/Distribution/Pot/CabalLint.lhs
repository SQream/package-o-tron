Cabal lint checks your cabal file

It currently reports:

missing other modules and build-deps (build-deps missing is usually
  hard to miss!)
superfluous other modules and build-deps
it checks library,exe, and v10 test sections
it also runs the packdeps check to check your upper bounds on the
  build-deps

TODO:
list files in project which aren't included in .cabal
verbose mode

run from another folder: deal with paths
run in folder with cabal and find it automatically without passing on command line

check with a bunch of packages on hackage

> -- | Code to check your .cabal file for some  issues
> {-# LANGUAGE TupleSections #-}
> module Distribution.Pot.CabalLint
>     (LintInfo(..)
>     ,SectionType(..)
>     ,cabalLint
>     ,ppLi
>     ) where


> import Distribution.PackDeps
> import Distribution.PackageDescription.Parse
> import Distribution.Verbosity
> import Distribution.PackageDescription
> import Control.Applicative
> import Data.List
> import Distribution.ModuleName hiding (main)
> import Distribution.Package
> import System.FilePath
> import Data.Maybe
> import qualified Data.Text as T
> import Distribution.Pot.Types
> import Distribution.Pot.RecursiveGetSources
> import Distribution.Pot.InstalledPackages
> import Distribution.Pot.DeepDependencies
> import System.Directory
> import Control.Arrow

> -- | Report on a .cabal file
> data LintInfo =
>     LI
>     {unmatchedImports :: [(FilePath,T.Text)] -- ^ filename, module name in the import of imports which don't match any local sources or any installed packages
>     ,ambiguousImports :: [(FilePath,T.Text)] -- ^ imports which match more than one local source and/or installed package
>     ,extraOtherModules :: [((SectionType,T.Text),[T.Text])] -- ^ section name (library, or exe name) plus list of modules in the other-module section which aren't needed
>     ,missingOtherModules :: [((SectionType,T.Text),[T.Text])] -- ^ modules (which correspond to local sources) which are missing fro the other-modules section
>     ,extraBuildDeps :: [((SectionType,T.Text),[T.Text])] -- ^ extra build-deps which aren't needed
>     ,missingBuildDeps :: [((SectionType,T.Text),[T.Text])] -- ^ build-deps which are mssing
>     ,wontAccept :: [(String,String)] -- ^ package name, version of packages which are too new for the version constraints in the .cabal
>     }
>     deriving Show

> data SectionType = Lib | Exe | Test --  | Benchmark
>                    deriving Show



> data SectionInformation =
>   SI
>   {siSrcDirs :: [FilePath]
>   ,siSectionType :: SectionType
>   ,siSectionName :: String
>   ,siModRoots :: [T.Text]
>   ,siOtherMods :: [T.Text]
>   ,siBuildDeps :: [T.Text]
>   }

> -- | Generate report for a .cabal file
> cabalLint :: FilePath -- ^ path to the .cabal file
>           -> [T.Text] -- ^ names of packages to hide
>           -> IO LintInfo
> cabalLint cabalFn hidePackages = do
>   -- read the .cabal file
>   di <- readPackageDescription normal cabalFn
>   --putStrLn $ groom di
>   let -- get the source dirs, add "." if empty list
>       -- not sure if "." should always be added?
>       sd bi = map (dropFileName cabalFn </>) $
>               let x = hsSourceDirs bi
>               in if null x
>                  then ["."]
>                  else x
>       -- get the info for the library section
>       libSi = do
>          cnd <- condLibrary di
>          let ctd = condTreeData cnd
>              srcDirs = sd $ libBuildInfo ctd
>          return SI
>                {siSrcDirs = srcDirs
>                ,siSectionType = Lib
>                ,siSectionName = dropExtension $ takeFileName cabalFn
>                ,siModRoots = map moduleString $ exposedModules ctd
>                ,siOtherMods = map moduleString $ otherModules $ libBuildInfo ctd
>                ,siBuildDeps = map (T.pack . dependencyName) $ condTreeConstraints cnd}
>       -- create the additional package for the library in the case that the exes refer to the library
>       libPi = fmap (\lsi -> PackageInfo {piName = T.pack $ siSectionName lsi
>                                         ,piExposedModules = siModRoots lsi
>                                         ,piDependencies = siBuildDeps lsi
>                                         }) libSi
>       -- exe sections
>       exeSis = flip map (condExecutables di) $ \(n,cnd) ->
>          let ctd = condTreeData cnd
>              srcDirs = sd $ buildInfo ctd
>          in SI {siSrcDirs = srcDirs
>                ,siSectionType = Exe
>                ,siSectionName = n
>                ,siModRoots = [T.pack $ dropExtension $ modulePath ctd]
>                ,siOtherMods = map moduleString $ otherModules $ buildInfo ctd
>                ,siBuildDeps = map (T.pack . dependencyName) $ condTreeConstraints cnd}
>       -- test sections
>       testsSis = flip map (condTestSuites di) $ \(n,cnd) ->
>          let ctd = condTreeData cnd
>              srcDirs = sd $ testBuildInfo ctd
>              modName = case testInterface ctd of
>                            TestSuiteExeV10 _ p -> [T.pack $ dropExtension p]
>                            x -> error $ "test suite type not supported: " ++ show x
>          in SI {siSrcDirs = srcDirs
>                ,siSectionType = Test
>                ,siSectionName = n
>                ,siModRoots = modName
>                ,siOtherMods = map moduleString $ otherModules $ testBuildInfo ctd
>                ,siBuildDeps = map (T.pack . dependencyName) $ condTreeConstraints cnd}
>       -- benchmark sections
>       benchmarksSis = [] -- TODO
>       sis = maybeToList libSi ++ exeSis ++ testsSis ++ benchmarksSis
>   -- do the checks
>   pkgs <- filter ((`notElem` hidePackages) . piName) <$> readPackages
>   (uis,ais,eos,mos,eds,mds) <- unzip6 <$> mapM (checkModulesAndPackages pkgs libPi) sis
>   badPvs <- checkNewestVersions cabalFn
>   let fi = filter (not . null . snd)
>   return $ LI (concat uis) (concat ais) (fi eos) (fi mos) (fi eds) (fi mds) badPvs
>   where
>     moduleString = T.pack . intercalate "." . components
>     dependencyName (Dependency (PackageName s) _) = s
>     getFilename :: [FilePath] -> T.Text -> IO FilePath
>     getFilename roots mn = do
>          let fnSuf = map (\x -> case x of
>                                   '.' -> '/'
>                                   _ -> x) $ T.unpack mn
>              mf r ext = do
>                let fn = r </> fnSuf `addExtension` ext
>                x <- doesFileExist fn
>                return $ if x
>                         then Just fn
>                         else Nothing
>          files <- catMaybes <$> sequence
>                   [mf r ext | r <- roots, ext <- ["hs","lhs"]]
>          case files of
>               [] -> error $ "file not found for " ++ T.unpack mn
>               [x] -> return x
>               xs -> error $ "ambiguous module name " ++ T.unpack mn ++ "\n" ++ show xs

>     symmDiff a b = let a' = nub a
>                        b' = nub b
>                    in (a' \\ b', b' \\ a')
>     checkModulesAndPackages pkgs libPi si = do
>         rootFns <- mapM (getFilename $ siSrcDirs si) (siModRoots si)
>         let thisPkgs = case (siSectionType si,libPi) of
>                          (Exe,Just p) -> p:pkgs
>                          _ -> pkgs
>         asd <- deepDependencies thisPkgs
>                <$> recursiveGetSources thisPkgs rootFns (siSrcDirs si)

unmatched imports

>         let unImports sd =
>                map ((sdFilename sd,) . fst) $ filter (null . snd) $ sdImports sd
>             umis :: [(FilePath, T.Text)]
>             umis = concatMap (unImports . ddSd) asd

ambiguous imports

>         let aImports sd =
>                map (sdFilename sd,) $ filter ((>1) . length . snd) $ sdImports sd
>             ambImports :: [(FilePath, T.Text)]
>             ambImports = concatMap (map (second fst) . aImports . ddSd) asd

other modules expected:
take all the modules in the asd list
filter out all the ones in the root
the names of the ones left is the expected list

>         let expectedOtherMods = filter (`notElem` siModRoots si)
>                                 $ mapMaybe (sdModuleName . ddSd) asd
>             (mo,eo) = symmDiff expectedOtherMods (siOtherMods si)

deps expected:
just nub the package deps of the whole asd list

>         let expectedDeps = sort $ nub $ concatMap (packageDeps . ddSd) asd
>             (md,ed) = symmDiff expectedDeps (siBuildDeps si)
>             pre = ((siSectionType si,T.pack $ siSectionName si),)

>         return (umis,ambImports,pre eo,pre mo,pre ed,pre md)

> checkNewestVersions :: FilePath -> IO [(String,String)]
> checkNewestVersions fp = do
>   newest <- loadNewest
>   mdi <- loadPackage fp
>   di2 <- case mdi of
>                  Just di2 -> return di2
>                  Nothing -> error $ "Could not parse cabal file: " ++ fp
>   case checkDeps newest di2 of
>             (_pn, _v, AllNewest) ->
>                 return []
>             (_pn, _v, WontAccept p _) ->
>                 --putStrLn "Cannot accept the following packages"
>                 return p

> -- | Rough show for information in LintInfo
> ppLi :: LintInfo -> String
> ppLi li = unlines $
>            showItems "unmatchedImports" sft (unmatchedImports li)
>            ++ showItems "ambiguousImports" sft (ambiguousImports li)
>            ++ showItems "extra other modules" sd (extraOtherModules li)
>            ++ showItems "missing other modules" sd (missingOtherModules li)
>            ++ showItems "extra build deps" sd (extraBuildDeps li)
>            ++ showItems "missing build deps" sd (missingBuildDeps li)
>            ++ showItems "won't accept packages" sv (wontAccept li)
>   where
>     sft (f,t) = f ++ ": " ++ T.unpack t
>     sd ((st,t),ds) = T.unpack t ++ "(" ++ show st ++ "): " ++ unlines (map T.unpack ds)
>     sv (p,v) = p ++ "-" ++ v
>     showItems :: String -> (a -> String) -> [a] -> [String]
>     showItems m r as =
>         if null as
>         then []
>         else m : map r as
