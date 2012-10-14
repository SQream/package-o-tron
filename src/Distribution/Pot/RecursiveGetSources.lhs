
> -- | Analyze the imports for a set of sources
> {-# LANGUAGE OverloadedStrings,TupleSections,ScopedTypeVariables #-}
> module Distribution.Pot.RecursiveGetSources (recursiveGetSources) where

> import Distribution.Pot.HaskellSourceParser
> import Distribution.Pot.Types

> import qualified Data.Text as T
> import qualified Data.Text.Lazy.IO as LT
> import System.FilePath
> import Control.Monad
> import Data.List
> import Distribution.Pot.InstalledPackages
> import Control.Monad.State
> import Control.Applicative
> import Data.Maybe
> import System.Directory
> import Control.Arrow

> -- | takes a set of source files (these should be your exposed modules and/or exe sources)
> -- and recurses through the imports to find all the local source files needed, and to
> -- note the package names for any imports which match installed packages
> recursiveGetSources :: [PackageInfo] -- ^ the return value from getPackages
>                     -> [FilePath] -- ^ the source files to start with
>                     -> [FilePath] -- ^ the folders which contain source files (same as the folders you pass to -i with ghc)
>                     -> IO [SourceDeps]
> recursiveGetSources pkgs rootSources rootFolders = do
>   -- read and parse the root sources
>   rs <- forM rootSources $ \fn -> do
>           f <- LT.readFile fn
>           -- todo: better error message
>           let i = either error id $ parseSource (sourceTypeOf fn) f
>               root = case siModuleName i of
>                        Nothing -> dropFileName fn
>                        Just m | m == "Main" -> dropFileName fn
>                        Just m -> let mfn = moduleNameToFileName m
>                                  in if isSuffixOf mfn $ dropExtension fn
>                                     then reverse . drop (length mfn) . reverse $ dropExtension fn
>                                     else error $ "module name doesn't match filename:\n"
>                                                  ++ T.unpack m ++ "\n" ++ fn ++ "\n" ++ mfn
>           return (fn,(root,i))
>   -- run the recursion
>   execStateT (mapM (uncurry (annotate pkgs rootFolders)
>                     . (second snd)) rs) []

> moduleNameToFileName :: T.Text -> FilePath
> moduleNameToFileName = map (\x -> case x of
>                                          '.' -> '/'
>                                          _ -> x) . T.unpack

1. parse a file:
reads the file
gets the list of imports and the module name
run through the imports:
memoized check for matching local source
 -> this also adds the local source to the running list
also check the package list

> type St = StateT [SourceDeps] IO

> annotate :: [PackageInfo] -> [FilePath] -> FilePath -> SourceImports -> St ()
> annotate pkgs roots fp ssi = do
>   iis <- forM (siImports ssi) $ \ip -> do
>            -- get the matching packages
>            let ims = map ImportFromPackage $ lookupPackageForModule pkgs ip
>            lfs <- afs ip
>            return (ip,sort $ nub $ lfs ++ ims)
>   let a = SourceDeps {sdFilename = fp
>                      ,sdModuleName = siModuleName ssi
>                      ,sdImports = sort $ nub iis}
>   modify (a:)
>   where
>     afs :: T.Text -> St [ImportInfo]
>     afs ip = do
>       -- if there is matching in the state, return that and we're done
>       st <- get
>       let memoed = flip mapMaybe st $ \assi ->
>                      case sdModuleName assi of
>                        Just m | m == ip -> Just $ sdFilename assi
>                        _ -> Nothing
>       if not (null memoed)
>         then return $ map ImportFromLocal memoed
>         else do
>       -- otherwise, try to find the file in all the roots
>       -- any matches -> load and annotate
>       let suf = moduleNameToFileName ip
>       concat <$> forM roots (\r -> do
>           a <- tryRead (r </> suf `addExtension` "hs")
>           b <- tryRead (r </> suf `addExtension` "lhs")
>           return $ catMaybes [a,b])
>     tryRead :: FilePath -> St (Maybe ImportInfo)
>     tryRead fn = do
>         --lift $ putStrLn $ "checking: " ++ fn
>         x <- lift $ doesFileExist fn
>         if x
>             then do
>                 newSrc <- lift $ LT.readFile fn
>                 let nssi = either error id $ parseSource (sourceTypeOf fn) newSrc
>                 annotate pkgs roots fn nssi
>                 return $ Just $ ImportFromLocal fn
>             else return Nothing
