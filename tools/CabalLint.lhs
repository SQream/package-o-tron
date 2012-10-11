
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
> import Control.Applicative
> import Data.List
> import Distribution.ModuleName hiding (main)
> import Control.Monad
> import Distribution.Package
> import System.FilePath
> import Data.Maybe
> import System.Exit
> import qualified Data.Text as T
> import Distribution.Pot.Types
> import Distribution.Pot.RecursiveGetSources
> import Distribution.Pot.InstalledPackages
> import Distribution.Pot.DeepDependencies
> import System.Directory

> data SectionType = Lib | Exe | Test --  | Benchmark

> data SectionInformation =
>   SI
>   {siSrcDirs :: [FilePath]
>   ,siSectionType :: SectionType
>   ,siSectionName :: String
>   ,siModRoots :: [T.Text]
>   ,siOtherMods :: [T.Text]
>   ,siBuildDeps :: [T.Text]
>   }

> data Opts =
>     Opts
>     {hidePackages :: [T.Text]
>     ,cabalFile :: FilePath
>     } deriving Show

todo: split out calc from main

make the calc as pure as possible with return value containing all
info instead of putstrln in middle of this code

> main :: IO ()
> main = do
>   opts <- parseArgs `fmap` getArgs
>   -- read the .cabal file
>   let fp = cabalFile opts
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
>                ,siModRoots = map moduleString $ exposedModules ctd
>                ,siOtherMods = map moduleString $ otherModules $ libBuildInfo ctd
>                ,siBuildDeps = map (T.pack . dependencyName) $ condTreeConstraints cnd}
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
>   pkgs <- filter ((`notElem` hidePackages opts) . piName) <$> readPackages
>   psGood <- and <$> mapM (checkModulesAndPackages pkgs) sis
>   vsGood <- checkNewestVersions fp
>   if psGood && vsGood
>       then exitSuccess
>       else exitFailure
>   where
>     moduleString = T.pack . intercalate "." . components
>     dependencyName (Dependency (PackageName s) _) = s
>     -- do symmetric difference of two lists
>     checkLists :: String -> String -> [T.Text] -> [T.Text] -> IO Bool
>     checkLists section field cab fil = do
>           let cab' = nub cab
>               fil' = nub fil
>               extraCab = cab' \\ fil'
>               missingCab = fil' \\ cab'
>           unless (null extraCab) $ putStrLn $ "extra unneeded " ++ field ++ " in " ++ section ++ ":\n" ++ intercalate "\n" (map T.unpack extraCab)
>           unless (null missingCab) $ putStrLn $ "missing " ++ field ++ " in " ++ section ++ ":\n" ++ intercalate "\n" (map T.unpack missingCab)
>           return $ null extraCab && null missingCab
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


>     checkModulesAndPackages pkgs si = do

TODO: if this is an exe section
and the packages list references the library section package name
and there is at least one module in the filesPackages which is
in the library exposed packages
then don't include the library package name in the list of cabalPackages
so it doesn't possibly show up as unneeded
and remove the any modules in the files modules which match
any of the exposed library modules
check the builddepends as well - if the package is only needed
by library modules then it shouldn't be listed

want to get the root filenames, only have the root module names
-> do wrapper

>         rootFns <- mapM (getFilename $ siSrcDirs si) (siModRoots si)
>         asd <- deepDependencies pkgs
>                <$> recursiveGetSources pkgs rootFns (siSrcDirs si)

other modules expected:
take all the modules in the asd list
filter out all the ones in the root
the names of the ones left is the expected list

>         let expectedOtherMods = filter (`notElem` siModRoots si)
>                                 $ mapMaybe (assiModuleName . dssiAssi) asd
>         a <- checkLists (siSectionName si) "other-modules"
>                         (siOtherMods si) expectedOtherMods

deps expected:
just nub the package deps of the whole asd list

write some wrappers to get the package imports from a

>         let expectedDeps = sort $ nub $ concatMap (packageDeps . dssiAssi) asd
>         b <- checkLists (siSectionName si) "build-depends"
>                         (siBuildDeps si) expectedDeps
>         return $ a && b

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


> parseArgs :: [String] -> Opts
> parseArgs s = do
>   f [] Nothing s
>   where
>     f ps c ("--hide-package":p:xs) =
>         f (p:ps) c xs
>     f _ps _c (["--hide-package"]) =
>         error $ "no package name after --hide-package"
>     f ps Nothing (x:xs) =
>         f ps (Just x) xs
>     f _ps (Just _) (_:_) =
>         error "please pass only one cabal file"
>     f ps (Just c) [] =
>         Opts (map T.pack ps) c
>     f _ps Nothing [] = error $ "please pass cabal file"

todo: run from another folder: deal with paths
run in folder with cabal and find it automatically
