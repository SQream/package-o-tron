
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
> import System.Process
> import System.Exit
> --import Text.Groom

> main :: IO ()
> main = do
>   opts <- parseArgs `fmap` getArgs
>   --putStrLn $ show opts
>   pkgs <- readPackages
>   let srcfs = if null (srcFolders opts)
>               then ["."]
>               else srcFolders opts
>   as <- recursiveGetSources pkgs (roots opts) srcfs
>   let asd = deepDependencies pkgs as
>   --putStrLn $ intercalate "\n" $ map groom asd
>   let allPacks = sort (nub $ concatMap ddDeepDeepPackages asd)
>                  \\ hidePackages opts
>   --putStrLn $ intercalate "\n" $ map T.unpack allPacks
>   tarballs <- availableTarballs
>   --putStrLn $ intercalate "\n" tarballs
>   let nonSysPacks = filter (`notElem` systemPackages) allPacks
>   wantTarballs <- mapM (findTarball (customPackages opts) tarballs)
>                        nonSysPacks
>   -- putStrLn $ intercalate "\n" wantTarballs
>   maybe (putStrLn $ intercalate "\n" wantTarballs)
>         (\f -> do
>            createDirectoryIfMissing True f
>            mapM_ (\t -> copyFile t (f </> takeFileName t)) wantTarballs)
>         $ outputFolder opts
>   where
>     findTarball cust l p = do
>       let cands = filter ((==p) . T.pack . dropVersion . takeBaseName) l
>           c = lookup p cust
>       case c of
>         Just c' -> do
>                save <- getCurrentDirectory
>                setCurrentDirectory c'
>                _ <- system "cabal configure"
>                x <- system "cabal sdist"
>                case x of
>                  ExitFailure _ -> error $ "error when running cabal sdist in " ++ c'
>                  _ -> return ()
>                tfn <- ((c' </> "dist") </>) `fmap` getTarball p "dist"
>                setCurrentDirectory save
>                return tfn
>         Nothing ->
>             case cands of
>                        [] -> error $ "no tarball for " ++ T.unpack p
>                        [x] -> return x
>                        -- todo: get the latest tarball, or get the installed version
>                        -- or something
>                        xs -> error $ "multiple tarballs for " ++ (T.unpack p) ++ "\n" ++ show xs
>     dropVersion = reverse . drop 1 . dropWhile (/='-') . reverse
>     getTarball nm' dir = do
>         let nm = T.unpack nm'
>         fs <- getDirectoryContents dir
>         case filter (\x -> ".tar.gz" `isSuffixOf` x
>                            && nm `isPrefixOf` x) fs of
>             [] -> error $ "couldn't find tarball in " ++ dir
>             [x] -> return x
>             xs -> error $ "multiple tarballs in " ++ dir ++ "\n" ++ show xs

> systemPackages :: [T.Text]
> systemPackages = ["Cabal","array","base","bin-package-db","binary"
>                  ,"bytestring","containers","deepseq","directory"
>                  ,"extensible-exceptions","filepath","ghc","ghc-prim"
>                  ,"haskell2010","haskell98","hoopl","hpc","integer-gmp"
>                  ,"old-locale","old-time","pretty","process","rts"
>                  ,"template-haskell","time","unix"]


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
>     ,customPackages :: [(T.Text,FilePath)]
>     } deriving Show

> parseArgs :: [String] -> Opts
> parseArgs = f [] [] [] Nothing []
>   where
>     f is ps rs o cs (x:xs) | "-i" `isPrefixOf` x =
>         let is' = splitOn ":" $ drop 2 x
>         in f (is ++ is') ps rs o cs xs
>     f is ps rs o cs ("--hide-package":p:xs) =
>         f is (p:ps) rs o cs xs
>     f _is _ps _rs _o _cs (["--hide-package"]) =
>         error "no package name after --hide-package"
>     f is ps rs Nothing cs ("--output-folder":o:xs) =
>         f is ps rs (Just o) cs xs
>     f is ps rs o cs ("--custom-package":nm:fn:xs) =
>         f is ps rs o ((T.pack nm,fn):cs) xs
>     f is ps rs o cs (x:xs) =
>         f is ps (x:rs) o cs xs
>     f is ps rs o cs [] =
>         Opts is (map T.pack ps) rs o cs
