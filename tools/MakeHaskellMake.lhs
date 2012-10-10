Example of calling this file:

MakeHaskellMake package-o-tron FLDS . exe-src EXES exe-src/Makefilerize exe-src/ShowPackages exe-src/CabalLint

write the packages to ignore first the the text 'FLDS'
then write the folders which source appears in (same folders you would
pass to -i with ghc) then the text 'EXES'
write the paths to the exes

It will generate rules to compile all the .o files and link the exes,
with explicit package lists.

See the included Makefile, autorules.mk file for an example of how to
use this, and an example of what is output.

TODO:

fix the interface:
--hide-package nm (multiple)
--source-root dir (multiple)
--source-file filepath (multiple)
so you have to specify the roots plus the source files?
or it could work out the roots automatically?
if you specify all the source files, it could work out which ones are
  mains automatically
maybe do as csv as well e.g. so can do --source-root=a,b,c

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

> import Distribution.Pot.Modules
> import Data.List
> import System.FilePath
> import System.Environment
> import Control.Arrow
> import Data.Char
> import Data.Maybe
> import Data.Time.Clock
> import Data.Time.Calendar

generate a makefile entry to compile a .lhs or .hs to .o and .hi
explicitly lists all the immediate .hi dependencies and package
  dependencies
specifies the -o explicitly since ghc outputs modules with a main as
  Main.o instead of their proper name (maybe this is a ghc bug?)

> moduleCompile :: ModuleInfo -> String
> moduleCompile mi =
>   objOf mi
>   ++ " : " ++ intercalate nl (miModuleFile mi : map (fhiOf . fst) (miLocalDeps mi))
>   ++ "\n\t-mkdir -p " ++ dropFileName (objOf mi)
>   ++ "\n\t$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ "
>   ++ nl ++ intercalate nl (map ("-package " ++) $ addBase $ miDirectPackages mi)
>   ++ nl ++ "-c $< -o " ++ objOf mi
>   ++ nl ++ "-i$(BUILD)/"

> nl :: String
> nl = " \\\n            "


> objOf :: ModuleInfo -> FilePath
> objOf = ("$(BUILD)/" ++) . flip replaceExtension "o" . miFilename
> fobjOf :: (FilePath,FilePath) -> FilePath
> fobjOf = ("$(BUILD)/" ++) . flip replaceExtension "o" . snd
> fhiOf :: (FilePath,FilePath) -> FilePath
> fhiOf = ("$(BUILD)/" ++) . flip replaceExtension "hi" . snd
> exeOf :: ModuleInfo -> FilePath
> exeOf = ("$(BUILD)/" ++) . takeFileName . dropExtension . miFilename

> addBase :: [String] -> [String]
> addBase x | "base" `elem` x = x
> addBase x = "base" : x

create the makefile entry to link a exe:
lists all the .o files needed in the link (doesn't use ghc --make),
and all the packages explicitly

TODO: try to split the lines a little less frequently

> exeLink :: ModuleInfo -> String
> exeLink mi =
>   exeOf mi ++ " : "
>   ++ intercalate nl (objOf mi : map (fobjOf . fst) (miLocalDeepDeps mi))
>   ++ "\n\t-mkdir -p $(BUILD)/"
>   ++ "\n\t$(HL) $(HL_OPTS) $(" ++ mangledExeName (miModuleFile mi)
>   ++ ") \\\n            "
>   ++ intercalate nl
>      (["-o " ++ exeOf mi, objOf mi]
>       ++ map (fobjOf . fst) (miLocalDeepDeps mi)
>       ++ ["-hide-all-packages"]
>       ++ map ("-package " ++) (addBase $ miDirectDeepPackages mi))
>   where
>     mangledExeName = (++ "_EXTRA")
>                      . map toUpper
>                      . map (\c -> case c of
>                                       '/' -> '_'
>                                       _ -> c)
>                      . takeFileName
>                      . dropExtension

> main :: IO ()
> main = do
>   args <- getArgs
>   let (hidepacks,args') = second (drop 1) $ break (=="FLDS") args
>       (modules,exes) = second (drop 1) $ break (=="EXES") args'
>   mis <- map (hidePackage hidepacks) `fmap` modulesInfo modules
>   (y,m,d) <- getCurrentTime >>= return . toGregorian . utctDay
>   putStrLn $ "# Autogenerated on " ++ show y ++ "/" ++ show m ++ "/" ++ show d ++ "\n\
>              \# http://hackage.haskell.org/package/package-o-tron\n"
>   putStrLn $ intercalate "\n\n" $ map moduleCompile mis
>   let exeMis = map exeMi exes
>       exeMi exe = fromMaybe
>                   (error $ "source file for exe not found: " ++ show exe
>                    ++ " in\n"
>                    ++ intercalate "\n" (map (dropExtension . miModuleFile) mis))
>                   $ find ((== exe) . dropExtension . miModuleFile) mis
>   putStrLn $ intercalate "\n\n" $ map exeLink exeMis
>   putStrLn "\n\n%.hi : %.o\n\t@:"
>   where
>     hidePackage ps m = m {miDirectPackages = filter (`notElem` ps) $ miDirectPackages m
>                          ,miDirectDeepPackages = filter (`notElem` ps) $ miDirectDeepPackages m}
