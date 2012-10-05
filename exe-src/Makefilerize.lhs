Example of calling this file:

Makefilerize package-o-tron FLDS . exe-src EXES exe-src/Makefilerize exe-src/ShowPackages exe-src/CabalLint

write the packages to ignore first the the text 'FLDS'
then write the folders which source appears in (same folders you would
pass to -i with ghc) then the text 'EXES'
write the paths to the exes

It will generate rules to compile all the .o files and link the exes,
with explicit package lists.

See the included Makefile, autorules.mk file for an example of how to
use this, and an example of what is output.

TODO:

support two stage .o, .dyn_o to get creation of .so with source that
uses template haskell

> import Distribution.Pot.Modules
> import Data.List
> import System.FilePath
> import System.Environment
> import Control.Arrow
> import Data.Char
> import Data.Maybe

> moduleCompile :: ModuleInfo -> String
> moduleCompile mi =
>   objOf mi
>   ++ " : " ++ intercalate " \\\n    " (miFileName mi : map (fhiOf . fst) (miLocalDependencies mi))
>   ++ "\n\t-mkdir -p " ++ dropFileName (objOf mi)
>   ++ "\n\t$(HC) $(HC_OPTS) -hide-all-packages -outputdir $(BUILD)/ "
>   ++ unwords (map ("-package " ++) $ addBase $ miPackages mi)
>   ++ " -c $< -o " ++ objOf mi
>   ++ " \\\n        -i$(BUILD)/"


> objOf :: ModuleInfo -> FilePath
> objOf = ("$(BUILD)/" ++) . flip replaceExtension "o" . miModuleFile
> fobjOf :: (FilePath,FilePath) -> FilePath
> fobjOf = ("$(BUILD)/" ++) . flip replaceExtension "o" . snd
> fhiOf :: (FilePath,FilePath) -> FilePath
> fhiOf = ("$(BUILD)/" ++) . flip replaceExtension "hi" . snd
> exeOf :: ModuleInfo -> FilePath
> exeOf = ("$(BUILD)/" ++) . takeFileName . dropExtension . miModuleFile

> addBase :: [String] -> [String]
> addBase x | "base" `elem` x = x
> addBase x = "base" : x

> exeLink :: ModuleInfo -> String
> exeLink mi =
>   exeOf mi ++ " : "
>   ++ intercalate " \\\n    " (objOf mi : map (fobjOf . fst) (miLocalTransitiveDependencies mi))
>   ++ "\n\t-mkdir -p $(BUILD)/"
>   ++ "\n\t$(HL) $(HL_OPTS) $(" ++ mangledExeName (miFileName mi)
>   ++ ") \\\n    "
>   ++ intercalate " \\\n    "
>      (["-o " ++ exeOf mi, objOf mi]
>       ++ map (fobjOf . fst) (miLocalTransitiveDependencies mi)
>       ++ ["-hide-all-packages"]
>       ++ map ("-package " ++) (addBase $ miTransitivePackages mi))
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
>   mis <- map (second $ hidePackage hidepacks) `fmap` modulesInfo modules
>   putStrLn $ intercalate "\n\n" $ map (moduleCompile . snd) mis
>   let exeMis = map exeMi exes
>       exeMi exe = fromMaybe
>                   (error $ "source file for exe not found: " ++ show exe
>                    ++ " in\n"
>                    ++ intercalate "\n" (map (dropExtension . miFileName . snd) mis))
>                   $ find ((== exe) . dropExtension . miFileName . snd) mis
>   putStrLn $ intercalate "\n\n" $ map (exeLink . snd) exeMis
>   putStrLn "\n\n%.hi : %.o\n\t@:"
>   where
>     hidePackage ps m = m {miPackages = filter (`notElem` ps) $ miPackages m
>                          ,miTransitivePackages = filter (`notElem` ps) $ miTransitivePackages m}

