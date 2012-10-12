
> import Distribution.Pot.RecursiveGetSources
> import System.Environment
> import Text.Groom
> import Distribution.Pot.InstalledPackages
> import Data.List
> import Data.List.Split

> main :: IO ()
> main = do
>   opts <- parseArgs `fmap` getArgs
>   pkgs <- readPackages
>   recursiveGetSources pkgs (roots opts) (srcFolders opts) >>= putStrLn . groom


> data Opts =
>     Opts
>     {srcFolders :: [FilePath]
>     ,roots :: [FilePath]
>     } deriving Show

> parseArgs :: [String] -> Opts
> parseArgs = f [] []
>   where
>     f is rs (x:xs) | "-i" `isPrefixOf` x =
>         let is' = splitOn ":" $ drop 2 x
>         in f (is ++ is') rs xs
>     f is rs (x:xs) =
>         f is (x:rs) xs
>     f is rs [] =
>         Opts is rs
