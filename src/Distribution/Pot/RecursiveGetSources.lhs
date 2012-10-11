
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

takes a set of root sources which are the files containing your mains
and your exposed modules, and an additional set of source roots if you
have source under different folders,
recursively follows all the imports, to create an output set of
information which contains all the local modules imported, with the
list of imports for each file parsed out and annotated with the local
file path or the name of the package the import refers to

By getting the package information also, it can report when it finds
an import which it doesn't recognise, or an import which could refer to
two source files, or two packages, or a package and a source file,
etc. For robustness: add multiple packages and emit warning, if local
source and one or more packages match, emit warning and use local
source only, and if two local sources match only then error?

> recursiveGetSources :: [PackageInfo] -> [FilePath] -> [FilePath] -> IO [AnnotatedSSI]
> recursiveGetSources pkgs rootSources rootFolders = do
>   -- read and parse the root sources
>   rs <- forM rootSources $ \fn -> do
>           f <- LT.readFile fn
>           -- todo: better error message
>           let i = either error id $ parseSource (sourceTypeOf fn) f
>               root = case ssiModuleName i of
>                        Nothing -> dropFileName fn
>                        Just m | m == "Main" -> dropFileName fn
>                        Just m -> let mfn = moduleNameToFileName m
>                                  in if isSuffixOf mfn $ dropExtension fn
>                                     then reverse . drop (length mfn) . reverse $ dropExtension fn
>                                     else error $ "module name doesn't match filename:\n"
>                                                  ++ T.unpack m ++ "\n" ++ fn ++ "\n" ++ mfn
>           return (fn,(root,i))
>   -- run the recursion
>   execStateT (mapM (uncurry $ annotate pkgs rootFolders)
>                     $ map (\(fn,(_,i)) -> (fn,i)) rs) []

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

> type St = StateT [AnnotatedSSI] IO

> annotate :: [PackageInfo] -> [FilePath] -> FilePath -> SourceSyntaxInfo -> St ()
> annotate pkgs roots fp ssi = do
>   iis <- forM (ssiImports ssi) $ \ip -> do
>            -- get the matching packages
>            let ims = map ImportFromPackage $ lookupPackageForModule pkgs ip
>            lfs <- afs ip
>            return (ip,sort $ nub $ lfs ++ ims)
>   let a = ASSI {assiFilename = fp
>                ,assiModuleName = ssiModuleName ssi
>                ,assiImports = sort $ nub iis}
>   modify (a:)
>   return ()
>   where
>     afs :: T.Text -> St [ImportInfo]
>     afs ip = do
>       -- if there is matching in the state, return that and we're done
>       st <- get
>       let memoed = flip mapMaybe st $ \assi ->
>                      case assiModuleName assi of
>                        Just m | m == ip -> Just $ T.pack $ assiFilename assi
>                        _ -> Nothing
>       if not (null memoed)
>         then return $ map ImportFromLocal memoed
>         else do
>       -- otherwise, try to find the file in all the roots
>       -- any matches -> load and annotate
>       let suf = moduleNameToFileName ip
>       concat <$> (forM roots $ \r -> do
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
>                 return $ Just $ ImportFromLocal (T.pack fn)
>             else return Nothing
