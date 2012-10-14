
todo:
add --use-base, --use-haskell98, --use-haskell2010 as shorthands for hiding the other two
and have --use-base as the default
hide the installed package with same name by default (no override at the moment)
then example of running on pot.cabal changes from:
CabalLint package-o-tron.cabal  --hide-package package-o-tron --hide-package haskell98 --hide-package haskell2010
to
CabalLint package-o-tron.cabal
deal with not being in the same folder as the .cabal file
deal with finding the cabal in the current folder by default so can run
CabaLint
on its own if you are in the right folder (maybe it can search upwards like e.g. git as well?)
deal with being pointed at a folder containing the .cabal file instead
of the .cabal file itself

> {-# LANGUAGE TupleSections #-}
> import System.Environment
> import qualified Data.Text as T
> import Distribution.Pot.CabalLint

> data Opts =
>     Opts
>     {hidePackages :: [T.Text]
>     ,cabalFile :: FilePath
>     } deriving Show

> main :: IO ()
> main = do
>   opts <- parseArgs `fmap` getArgs
>   cabalLint (cabalFile opts) (hidePackages opts) >>= putStrLn . ppLi

> parseArgs :: [String] -> Opts
> parseArgs = f [] Nothing
>   where
>     f ps c ("--hide-package":p:xs) =
>         f (p:ps) c xs
>     f _ps _c (["--hide-package"]) =
>         error "no package name after --hide-package"
>     f ps Nothing (x:xs) =
>         f ps (Just x) xs
>     f _ps (Just _) (_:_) =
>         error "please pass only one cabal file"
>     f ps (Just c) [] =
>         Opts (map T.pack ps) c
>     f _ps Nothing [] = error "please pass cabal file"
