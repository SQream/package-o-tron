Example of calling this file:

MakeHaskellMake --hide-package package-o-tron --hide-package haskell2010 --hide-package haskell98 -isrc:tools tools/*.lhs

It will generate rules to compile all the .o files and link the exes,
with explicit package lists.

See the included Makefile, autorules.mk file for an example of how to
use this, and an example of what is output.


> {-# LANGUAGE OverloadedStrings,TupleSections #-}
> import Distribution.Pot.MakefileGen
> import Data.List
> import System.Environment
> import Data.Time.Clock
> import Data.Time.Calendar
> import Data.List.Split
> import qualified Data.Text as T

> data Opts =
>     Opts
>     {srcFolders :: [FilePath]
>     ,hidePackages :: [T.Text]
>     ,roots :: [FilePath]
>     } deriving Show

> parseArgs :: [String] -> Opts
> parseArgs = f [] [] []
>   where
>     f is ps rs (x:xs) | "-i" `isPrefixOf` x =
>         let is' = splitOn ":" $ drop 2 x
>         in f (is ++ is') ps rs xs
>     f is ps rs ("--hide-package":p:xs) =
>         f is (p:ps) rs xs
>     f  _is _ps _rs (["--hide-package"]) =
>         error "no package name after --hide-package"
>     f is ps rs (x:xs) =
>         f is ps (x:rs) xs
>     f is ps rs [] =
>         Opts is (map T.pack ps) rs


> main :: IO ()
> main = do
>   opts <-parseArgs `fmap` getArgs
>   --pkgs <- readPackages
>   mfg <- makefileGen (srcFolders opts) (roots opts) (hidePackages opts)
>   -- write small note
>   (y,m,d) <- (toGregorian . utctDay) `fmap` getCurrentTime
>   putStrLn $ "# Autogenerated on " ++ show y ++ "/" ++ show m ++ "/" ++ show d ++ "\n\
>              \# http://hackage.haskell.org/package/package-o-tron\n"
>   putStrLn $ ppMakefile mfg
