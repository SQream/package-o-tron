
pass an optional -i source list
and the source roots (the exe files/ exposed modules or both)
and it will list the tarballs for all the packages needed for these sources
by ignoring the packages which come with ghc, and looking in your
$HOME/.cabal folder for the tarballs of the remaining packages
if you pass an --output-folder, it will copy the tarballs here
the idea is you can set up a fresh system which can compile all the code
with a copy of ghc, cabal-install and the tarballs without needing to
connect to hackage, and with all the versions being the ones on your
current system also.

> {-# LANGUAGE OverloadedStrings #-}
> import System.Environment
> import qualified Data.Text as T
> import Data.List.Split
> import Data.List
> import Distribution.Pot.RecursiveGetSources
> import Distribution.Pot.InstalledPackages
> import Distribution.Pot.Types
> import Distribution.Pot.DeepDependencies
> import qualified System.FilePath.Find as F
> import System.FilePath.Find ((==?))
> import System.Directory
> import System.FilePath
> --import Text.Groom

> main :: IO ()
> main = do
>   opts <- parseArgs `fmap` getArgs
>   pkgs <- readPackages
>   let srcfs = if null (srcFolders opts)
>               then ["."]
>               else srcFolders opts
>   as <- recursiveGetSources pkgs (roots opts) srcfs
>   let asd = deepDependencies pkgs as
>   --putStrLn $ intercalate "\n" $ map groom asd
>   let allPacks = (sort $ nub $ concatMap dssiDeepDeepPackages asd)
>                  \\ hidePackages opts
>   --putStrLn $ intercalate "\n" $ map T.unpack allPacks
>   tarballs <- availableTarballs
>   --putStrLn $ intercalate "\n" tarballs
>   let nonSysPacks = filter (`notElem` systemPackages) allPacks
>   let wantTarballs = map (findTarball tarballs) $ map T.unpack nonSysPacks
>   -- putStrLn $ intercalate "\n" wantTarballs
>   maybe (putStrLn $ intercalate "\n" wantTarballs)
>         (\f -> do
>            createDirectoryIfMissing True f
>            mapM_ (\t -> copyFile t (f </> takeFileName t)) wantTarballs)
>         $ outputFolder opts
>   where
>     findTarball l p =
>       let cands = filter ((==p) . dropVersion . takeBaseName) l
>       in case cands of
>            [] -> error $ "no tarball for " ++ p
>            [x] -> x
>            -- todo: get the latest tarball, or get the installed version
>            -- or something
>            xs -> error $ "multiple tarballs for " ++ p ++ "\n" ++ show xs
>     dropVersion = reverse . drop 1 . dropWhile (/='-') . reverse

TODO: maybe it should check if there is a newer tarball for a system
package and emit a warning or error out if there is

> systemPackages :: [T.Text]
> systemPackages = ["Cabal","array","base","bin-package-db","binary","bytestring","containers","deepseq","directory","extensible-exceptions","filepath","ghc","ghc-prim","haskell2010","haskell98","hoopl","hpc","integer-gmp","old-locale","old-time","pretty","process","rts","template-haskell","time","unix"]


> availableTarballs :: IO [FilePath]
> availableTarballs = do
>   h <- getHomeDirectory
>   F.find (return True) (F.extension ==? ".gz") (h </> ".cabal")


> data Opts =
>     Opts
>     {srcFolders :: [FilePath]
>     ,hidePackages :: [T.Text]
>     ,roots :: [FilePath]
>     ,outputFolder :: Maybe FilePath
>     } deriving Show

> parseArgs :: [String] -> Opts
> parseArgs s = do
>   f Nothing [] [] [] s
>   where
>     f o is ps rs (x:xs) | "-i" `isPrefixOf` x =
>         let is' = splitOn ":" $ drop 2 x
>         in f o (is ++ is') ps rs xs
>     f o is ps rs ("--hide-package":p:xs) =
>         f o is (p:ps) rs xs
>     f _o _is _ps _rs (["--hide-package"]) =
>         error $ "no package name after --hide-package"
>     f Nothing is ps rs ("--output-folder":o:xs) =
>         f (Just o) is ps rs xs
>     f o is ps rs (x:xs) =
>         f o is ps (x:rs) xs
>     f o is ps rs [] =
>         Opts is (map T.pack ps) rs o
