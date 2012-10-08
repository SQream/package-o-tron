
Cabal lint checks your cabal file

It currently reports:

missing other modules and build-deps (build-deps missing is usually
  hard to miss!)
superfluous other modules and build-deps
it checks library,exe, and v10 test sections
it also runs the packdeps check to check your upper bounds on the
  build-deps


TODO:
check for duplicates in other-modules and build-depends
  and other-modules include root modules (already does this?)
list files in project which aren't included in .cabal
verbose mode

deal with case when executables, etc refer to the package in their
dependencies (package-o-tron.cabal itself has this issue)

> import System.Environment
> import Distribution.PackDeps
> import Distribution.PackageDescription.Parse
> import Distribution.Verbosity
> import Distribution.PackageDescription
> import Distribution.Pot.Modules
> import Control.Applicative
> import Data.List
> import Distribution.ModuleName hiding (main)
> import Control.Monad
> import Distribution.Package
> import System.FilePath
> import Data.Maybe
> import System.Exit

> data SectionType = Lib | Exe | Test --  | Benchmark

> data SectionInformation =
>   SI
>   {siSrcDirs :: [FilePath]
>   ,siSectionType :: SectionType
>   ,siSectionName :: String
>   ,siModRoots :: [ModuleName]
>   ,siOtherMods :: [ModuleName]
>   ,siBuildDeps :: [Dependency]
>   }

> main :: IO ()
> main = do
>   [fp] <- getArgs
>   -- read the .cabal file
>   di <- readPackageDescription normal fp
>   --putStrLn $ groom di
>   let -- get the source dirs, add "." if empty list
>       -- not sure if "." should always be added?
>       sd bi = map (dropFileName fp </>) $
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
>                ,siSectionName = dropExtension $ takeFileName fp
>                ,siModRoots = exposedModules ctd
>                ,siOtherMods = otherModules $ libBuildInfo ctd
>                ,siBuildDeps = condTreeConstraints cnd}
>       -- exe sections
>       exeSis = flip map (condExecutables di) $ \(n,cnd) ->
>          let ctd = condTreeData cnd
>              srcDirs = sd $ buildInfo ctd
>          in SI {siSrcDirs = srcDirs
>                ,siSectionType = Exe
>                ,siSectionName = n
>                ,siModRoots = [fromString $ dropExtension $ modulePath ctd]
>                ,siOtherMods = otherModules $ buildInfo ctd
>                ,siBuildDeps = condTreeConstraints cnd}
>       -- test sections
>       testsSis = flip map (condTestSuites di) $ \(n,cnd) ->
>          let ctd = condTreeData cnd
>              srcDirs = sd $ testBuildInfo ctd
>              modName = case testInterface ctd of
>                            TestSuiteExeV10 _ p -> [fromString $ dropExtension p]
>                            x -> error $ "test suite type not supported: " ++ show x
>          in SI {siSrcDirs = srcDirs
>                ,siSectionType = Test
>                ,siSectionName = n
>                ,siModRoots = modName
>                ,siOtherMods = otherModules $ testBuildInfo ctd
>                ,siBuildDeps = condTreeConstraints cnd}
>       -- benchmark sections
>       benchmarksSis = [] -- TODO
>       sis = maybeToList libSi ++ exeSis ++ testsSis ++ benchmarksSis
>   -- get all the module info for all the sourcedirs in all the sections
>   allMis <- modulesInfo $ nub $ concatMap siSrcDirs sis
>   -- do the checks
>   psGood <- and <$> mapM (checkModulesAndPackages allMis) sis
>   vsGood <- checkNewestVersions fp
>   if psGood && vsGood
>       then exitSuccess
>       else exitFailure
>   where
>     moduleString = intercalate "." . components
>     dependencyName (Dependency (PackageName s) _) = s
>     -- do symmetric difference of two lists
>     checkLists section field cab fil = do
>           let cab' = nub cab
>               fil' = nub fil
>               extraCab = cab' \\ fil'
>               missingCab = fil' \\ cab'
>           unless (null extraCab) $ putStrLn $ "extra unneeded " ++ field ++ " in " ++ section ++ ":\n" ++ intercalate "\n" extraCab
>           unless (null missingCab) $ putStrLn $ "missing " ++ field ++ " in " ++ section ++ ":\n" ++ intercalate "\n" missingCab
>           return $ null extraCab && null missingCab
>     checkModulesAndPackages allMis si = do
>          -- TODO: if this is an exe section
>          -- and the packages list references the library section package name
>          -- and there is at least one module in the filesPackages which is
>          -- in the library exposed packages
>          -- then don't include the library package name in the list of cabalPackages
>          -- so it doesn't possibly show up as unneeded
>          -- and remove the any modules in the files modules which match
>          -- any of the exposed library modules
>          -- check the builddepends as well - if the package is only needed
>          -- by library modules then it shouldn't be listed
>          let rootModules = map moduleString $ siModRoots si
>              mis = filter ((`elem` rootModules) . miModuleName) allMis
>              filesModules = sort $ nub $ concatMap (map snd . miLocalDeepDeps) mis
>              filesPackages = sort $ nub $ concatMap (miDirectDeepPackages) mis
>              cabalOms = map moduleString (siOtherMods si)
>              cabalPackages = map dependencyName (siBuildDeps si)
>          a <- checkLists (siSectionName si) "other-modules" cabalOms (filesModules \\ rootModules)
>          b <- checkLists (siSectionName si) "build-depends" cabalPackages filesPackages
>          return $ a && b

> checkNewestVersions :: FilePath -> IO Bool
> checkNewestVersions fp = do
>   newest <- loadNewest
>   mdi <- loadPackage fp
>   di2 <- case mdi of
>                  Just di2 -> return di2
>                  Nothing -> error $ "Could not parse cabal file: " ++ fp
>   case checkDeps newest di2 of
>             (_pn, _v, AllNewest) ->
>                 return True
>             (_pn, _v, WontAccept p _) -> do
>                 putStrLn "Cannot accept the following packages"
>                 forM_ p $ \(x, y) -> putStrLn $ x ++ " " ++ y
>                 return False
