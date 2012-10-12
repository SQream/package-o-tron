

> {-# LANGUAGE OverloadedStrings,TupleSections #-}
> module Distribution.Pot.MakefileGen
>     (MakefileGen(..)
>     ,CompileModule(..)
>     ,ExeLink(..)
>     ,makefileGen
>     ,ppMakefile
>     ) where

TODO:

if you get a project, and you are missing a bunch of the packages
needed, you can run make and it will tell you the missing packages
drip by drip. if it is quick, have it check all the packages needed
for a target automatically and report them all before trying to
compile, or if this check is slow, add a new makefile target which
will report all the files and which set of packages they need and
which of these packages are missing in one go

work on robustness:
what if a file is missing
can't be parsed
what if a module imports something unrecognised
A module appears twice with the same name
Had problems with a module having the wrong name -> check this at
parse time?
modules appear in sources as well as packages, or appears in two
  different packages
two different versions of the same package are installed

etc.
want to be able to emit a warning and muddle through when this happens

support two stage .o, .dyn_o to get creation of .so with source that
uses template haskell

situation: you have some new module code written, you want to run make
autorules, but there is a syntax error which stops the autorules from
building. The error messages from here are a bit crap. ?solution: if
this file can't parse a source, maybe try and parse it with ghc to get
better syntax errors before the autorules can be built.


what happens when it finds an import which it can't track down, want a
good error message in this case also for same reason

> import Data.List
> import System.FilePath
> import Control.Arrow
> import Data.Char
> import Distribution.Pot.Types
> import Distribution.Pot.RecursiveGetSources
> import Distribution.Pot.InstalledPackages
> import Distribution.Pot.DeepDependencies
> import qualified Data.Text as T
> import Data.Either

generate a makefile entry to compile a .lhs or .hs to .o and .hi
explicitly lists all the immediate .hi dependencies and package
  dependencies
specifies the -o explicitly since ghc outputs modules with a main as
  Main.o instead of their proper name (maybe this is a ghc bug?)
  -> it only does this when you use an outputdir

> -- | information needed to compile a module to .o
> data CompileModule =
>     CompileModule
>     {cmHsName :: FilePath -- ^ the .hs/.lhs file for the module
>     ,cmObjName :: FilePath -- ^ the .o file for the module
>     ,cmDependencies :: [FilePath] -- ^ the dependencies (.lhs and the .hi of the local imports)
>     ,cmPackages :: [T.Text] -- ^ the packages needed to compile this module
>     }

> compileModule :: [T.Text] -> DeepSourceDeps -> CompileModule
> compileModule hidePacks dssi =
>     let (modDeps,packDeps) = partitionEithers $
>                              concatMap(\(n,i) -> map (either (Left . (n,)) (Right . (n,)) . splitDep) i)
>                              $ sdImports $ ddSd dssi
>         pds = sort (nub $ map snd packDeps) \\ hidePacks
>         assi = ddSd dssi
>     in CompileModule (sdFilename assi)
>                      (objOf (sdModuleName assi, sdFilename assi))
>                      (sdFilename (ddSd dssi)
>                       : map (hiOf . first Just) modDeps)
>                      pds

> fOf :: String -> (Maybe T.Text,FilePath) -> FilePath
> fOf e (Nothing,fp) = ("$(BUILD)" </>) . (`addExtension` e) . takeBaseName $ fp
> fOf e (Just m, _fp) = ("$(BUILD)" </>) . (`addExtension` e) . mfn $ T.unpack m
>     where
>       mfn = map (\c -> case c of
>                               '.' -> '/'
>                               _ -> c)
> hiOf,objOf :: (Maybe T.Text,FilePath) -> FilePath
> hiOf = fOf "hi"
> objOf = fOf "o"


> ppCM :: CompileModule -> String
> ppCM cm =
>     cmObjName cm ++ " : "
>     ++ intercalate nl (cmDependencies cm)
>     ++ "\n\t-mkdir -p " ++ dropFileName (cmObjName cm)
>     ++ "\n\t$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ "
>     ++ nl ++ intercalate nl (map (("-package " ++) . T.unpack) $ cmPackages cm)
>     ++ nl ++ "-c " ++ cmHsName cm
>     ++ nl ++ "-o " ++ cmObjName cm
>     ++ nl ++ "-i$(BUILD)/"

> nl :: String
> nl = " \\\n            "

create the makefile entry to link a exe:
lists all the .o files needed in the link (doesn't use ghc --make),
and all the packages explicitly

> -- | info needed to link an .o to an executable
> data ExeLink =
>     ExeLink
>     {elExeName :: FilePath -- ^ the name of the exe file
>     ,elMangledExeName :: String -- ^ the mangled name (used to supply exe specific options from the handwritten part of your makefile)
>     ,elObjects :: [FilePath] -- ^ the list of .o files to link in
>     ,elPackages :: [T.Text] -- ^ all the packages needed to link the exe
>     }

TODO: try to split the lines a little less frequently

> ppEL :: ExeLink -> String
> ppEL el =
>   elExeName el ++ " : "
>   ++ intercalate nl (elObjects el)
>   ++ "\n\t-mkdir -p $(BUILD)/"
>   ++ "\n\t$(HL) $(HL_OPTS) $(" ++ elMangledExeName el
>   ++ ") \\\n            "
>   ++ intercalate nl
>      (["-o " ++ elExeName el]
>       ++ elObjects el
>       ++ map (("-package " ++) . T.unpack) (elPackages el))

> exeLink :: [T.Text] -> DeepSourceDeps -> ExeLink
> exeLink hidePacks dssi =
>     let exeName = takeBaseName $ sdFilename $ ddSd dssi
>         (modDeps,packDeps) = partitionEithers $
>                              concatMap(\(n,i) ->
>                                   map (either (Left . (n,)) (Right . (n,)) . splitDep) i)
>                              $ ddDeepImports dssi
>         pds = sort (nub $ map snd packDeps) \\ hidePacks
>         assi = ddSd dssi

>     in ExeLink {elExeName = "$(BUILD)" </> exeName
>                ,elMangledExeName = mangledExeName exeName
>                ,elObjects = objOf (sdModuleName assi, sdFilename assi)
>                             : map (objOf . first Just) modDeps
>                ,elPackages = pds}
>     where
>         mangledExeName = (++ "_EXTRA")
>                      . map (toUpper
>                             . (\c -> case c of
>                                       '/' -> '_'
>                                       _ -> c))
>                      . takeFileName
>                      . dropExtension

> -- | Represents the information needed to compile and link a bunch of haskell source files
> data MakefileGen =
>     MakefileGen
>     {compiles :: [CompileModule]
>     ,links :: [ExeLink]
>     }

> -- | convert the MakefileGen info to Makefile concrete syntax
> ppMakefile :: MakefileGen -> String
> ppMakefile mfg =
>   unlines
>   [intercalate "\n\n" $ map ppCM $ compiles mfg
>   ,intercalate "\n\n" $ map ppEL $ links mfg
>   ,"\n\n%.hi : %.o\n\t@:"]

> -- | Analyze a set of files and produce the information needed to generate a makefile
> makefileGen :: [FilePath] -- ^ the folders where the source lives (same as -i for ghc)
>             -> [FilePath] -- ^ the root sources, i.e. your exposed modules and/or mains
>             -> [T.Text] -- ^ list of package names to hide
>             -> IO MakefileGen
> makefileGen srcFolders roots hidePackages = do
>   pkgs <- readPackages
>   let srcfs = if null srcFolders
>               then ["."]
>               else srcFolders
>   -- parse all the sources
>   asd <- deepDependencies pkgs `fmap` recursiveGetSources pkgs roots srcfs
>   return MakefileGen
>          {compiles = map (compileModule hidePackages) asd
>          ,links = map (exeLink hidePackages)
>                   $ filter ((`elem` [Just "Main", Nothing])
>                             . sdModuleName . ddSd) asd}
