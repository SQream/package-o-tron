
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

> import System.Environment
> import Distribution.PackDeps
> import Distribution.PackageDescription.Parse
> import Distribution.Verbosity
> import Distribution.PackageDescription
> import Development.Pot.Modules
> import Control.Applicative
> import Data.List
> import Distribution.ModuleName hiding (main)
> import Control.Monad
> import Distribution.Package
> import System.FilePath
> import Data.Maybe
> import System.Exit

> data SectionInformation =
>   SI
>   {siSrcDirs :: [FilePath]
>   ,siSectionName :: String
>   ,siModRoots :: [ModuleName]
>   ,siOtherMods :: [ModuleName]
>   ,siBuildDeps :: [Dependency]
>   }

> main :: IO ()
> main = do
>   [fp] <- getArgs
>   di <- readPackageDescription normal fp
>   --putStrLn $ groom di
>   let -- get the source dirs, add "." if empty list
>       -- not sure if "." should always be added?
>       sd bi = map (dropFileName fp </>) $
>               let x = hsSourceDirs bi
>               in if null x
>                  then ["."]
>                  else x
>       libSi = do
>          cnd <- condLibrary di
>          let ctd = condTreeData cnd
>              srcDirs = sd $ libBuildInfo ctd
>          return $ SI srcDirs "library" (exposedModules ctd)
>                         (otherModules $ libBuildInfo ctd)
>                         (condTreeConstraints cnd)
>       exeSis = flip map (condExecutables di) $ \(n,cnd) ->
>          let ctd = condTreeData cnd
>              srcDirs = sd $ buildInfo ctd
>          in SI srcDirs ("Executable: " ++ n)
>                        [fromString $ dropExtension $ modulePath ctd]
>                        (otherModules $ buildInfo ctd)
>                        (condTreeConstraints cnd)
>       testsSis = flip map (condTestSuites di) $ \(n,cnd) ->
>          let ctd = condTreeData cnd
>              srcDirs = sd $ testBuildInfo ctd
>              modName = case testInterface ctd of
>                            TestSuiteExeV10 _ p -> [fromString $ dropExtension p]
>                            x -> error $ "test suite type not supported: " ++ show x
>          in SI srcDirs ("Executable: " ++ n)
>                        modName
>                        (otherModules $ testBuildInfo ctd)
>                        (condTreeConstraints cnd)
>       benchmarksSis = [] -- TODO
>       sis = maybeToList libSi ++ exeSis ++ testsSis ++ benchmarksSis

>   allMis <- modulesInfo $ nub $ concatMap siSrcDirs sis
>   psGood <- and <$> mapM (checkModulesAndPackages allMis) sis
>   vsGood <- checkNewestVersions fp
>   if psGood && vsGood
>       then exitSuccess
>       else exitFailure
>   where
>     moduleString = intercalate "." . components
>     dependencyName (Dependency (PackageName s) _) = s
>     checkLists section field cab fil = do
>           let cab' = nub cab
>               fil' = nub fil
>               extraCab = cab' \\ fil'
>               missingCab = fil' \\ cab'
>           unless (null extraCab) $ putStrLn $ "extra unneeded " ++ field ++ " in " ++ section ++ ":\n" ++ intercalate "\n" extraCab
>           unless (null missingCab) $ putStrLn $ "missing " ++ field ++ " in " ++ section ++ ":\n" ++ intercalate "\n" missingCab
>           return $ null extraCab && null missingCab
>     checkModulesAndPackages allMis si = do
>          let rootModules = map moduleString $ siModRoots si
>              mis = filter ((`elem` rootModules) . miModuleName . snd) allMis
>              filesModules = (sort $ nub $ concatMap (map snd . miLocalTransitiveDependencies . snd) mis)
>              filesPackages = sort $ nub $ concatMap (miTransitivePackages . snd) mis
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
>   allGood <- case checkDeps newest di2 of
>             (_pn, _v, AllNewest) -> do
>                 return True
>             (_pn, _v, WontAccept p _) -> do
>                 putStrLn "Cannot accept the following packages"
>                 forM_ p $ \(x, y) -> putStrLn $ x ++ " " ++ y
>                 return False
>   return allGood
