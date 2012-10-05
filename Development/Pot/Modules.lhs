
simplistic parser which can list the modules imported by a haskell
source file

provides a recursive-memoized mode which can efficiently get the
complete transitive imports of a set of files

provides a method of splitting the imports into modules which are in
the local fileset, and modules which aren't (the local module
dependencies can be used for a build tool, and the non-local ones for
determining which packages are referenced)

> {-# LANGUAGE TupleSections #-}
> module Development.Pot.Modules
>     (ModuleInfo(..)
>     ,modulesInfo
>     ,showmi
>     ,filterModules) where

> import Data.Maybe
> import qualified System.FilePath.Find as F
> import System.FilePath.Find ((==?),(||?))
> import System.FilePath
> import Text.Groom
> import Data.List
> import Development.Pot.Packages
> import Control.Arrow

> parseImports :: String -> [String]
> parseImports src = mapMaybe (im . words) $ lines src
>   where
>     im (">":x:_) | x /= "import" = Nothing
>     im (">":"import":"qualified":nm:_) = Just nm
>     im (">":"import":nm:_) = Just nm
>     im (x:_) | x /= "import" = Nothing
>     im ("import":"qualified":nm:_) = Just nm
>     im ("import":nm:_) = Just nm
>     im _ = Nothing

reads the folders passed in as source roots and returns a list of all
the modules

> getLocalModules :: [FilePath] -> IO [(FilePath,FilePath)]
> getLocalModules dirs =
>   concat `fmap` mapM findHs dirs
>   where
>     findHs dir =
>        map ((dir,) . makeRelative dir) `fmap`
>        F.find (return True) (F.extension ==? ".lhs" ||? F.extension ==? ".hs") dir

> data ModuleInfo =
>     ModuleInfo
>     {miFileName :: FilePath
>     ,miFilePrefix :: FilePath
>     ,miModuleFile :: FilePath
>     ,miModuleName :: String
>     ,miLocalDependencies :: [((FilePath,FilePath),String)]
>     ,miLocalTransitiveDependencies :: [((FilePath,FilePath),String)]
>     ,miPackages :: [String]
>     ,miTransitivePackages :: [String]
>     } deriving Show

> modulesInfo :: [FilePath] -> IO [(FilePath,ModuleInfo)]
> modulesInfo srcs = do
>   lms <- getLocalModules srcs
>   mps <- mapM (\(a,b) -> ((a,b),) `fmap` parseImports `fmap` readFile (a </> b)) lms
>   -- mps is a map from (sourceroot,modulepath) to [modules imported]
>   --putStrLn $ intercalate "\n\n" $ map (\(n,is) -> show n ++ "\n" ++ groom is) mps
>   -- split mps 2nd elements into local modules and other modules
>   let localModuleFiles = map (snd . fst) mps
>       localModuleNames = map (repSlash . dropExtension) localModuleFiles
>   --putStrLn $ groom localModuleNames
>   let mps2 = map (second (partition (`elem` localModuleNames))) mps
>   --putStrLn $ intercalate "\n\n" $ map (\(n,is) -> show n ++ "\n" ++ groom is) mps2
>   -- process the other module lists to get list of packages instead
>   pkgs <- readPackages
>   let mps3 = map (second $ second $ getPackages pkgs) mps2
>       tms = map mm mps3
>   -- add the transitive local dependencies
>       ldeps = map (\((_,mf),(lds,_)) ->
>                      (repSlash (dropExtension mf), lds)) mps3
>       allDeps :: String -> [String]
>       allDeps f = sort $ nub
>                   $ maybe []
>                   (\x -> x ++ concatMap allDeps x)
>                   $ lookup f ldeps
>       tmslds = map (second $ \m -> m {miLocalTransitiveDependencies =
>                                       map (("",""),) $ allDeps (miModuleName m)}) tms
>   -- add the transitive packages
>       packm :: [(String,[String])]
>       packm = map (\((_,mf),(_,ps)) ->
>                      (repSlash (dropExtension mf), ps)) mps3
>       packs s = fromMaybe [] $ lookup s packm
>       tmspacks = map (second $ \m -> m {miTransitivePackages =
>                                         sort $ nub
>                                         $ concatMap packs (miModuleName m
>                                                            : map snd (miLocalTransitiveDependencies m))})
>                      tmslds
>       lmFile (_,m) =
>           maybe (error $ "couldn't get filename for " ++ m)
>           (\(r,f) -> ((r, f), m))
>           $ find ((==m) . repSlash . dropExtension . snd) lms
>       ff x = x {miLocalDependencies = map lmFile $ miLocalDependencies x
>                ,miLocalTransitiveDependencies = map lmFile $ miLocalTransitiveDependencies x}
>   --putStrLn $ intercalate "\n\n" $ map (\(n,is) -> show n ++ "\n" ++ groom is) mps3
>       t2 = map (second ff) tmspacks
>   return t2
>   where
>     mm ((r,f),(lds,ps)) = (r </> f
>                           ,ModuleInfo {miFileName = r </> f
>                                       ,miFilePrefix = r
>                                       ,miModuleFile = f
>                                       ,miModuleName = repSlash (dropExtension f)
>                                       ,miLocalDependencies = map (("",""),) lds
>                                       ,miLocalTransitiveDependencies = []
>                                       ,miPackages = ps
>                                       ,miTransitivePackages = []})
>     repSlash = map $ \c -> case c of
>                              '/' -> '.'
>                              _ -> c
>     getPackages :: [(String,[String])] -> [String] -> [String]
>     getPackages pkgs ms =
>        let pkg m = fst `fmap` find (\(_,pms) -> m `elem` pms) pkgs
>            p1 = mapMaybe pkg ms
>        in sort $ nub p1

> filterModules :: (String -> Bool) -> ModuleInfo -> ModuleInfo
> filterModules f mi = mi {miPackages = filter f $ miPackages mi
>                         ,miTransitivePackages = filter f $ miTransitivePackages mi}


> showmi :: [(FilePath,ModuleInfo)] -> String
> showmi = intercalate "\n\n" . map (\(f,i) -> f ++ "\n" ++ groom i)
