
> import Distribution.Pot.RecursiveGetSources
> import System.Environment
> --import Control.Monad
> import Text.Groom
> --import Distribution.Pot.Types
> import  Distribution.Pot.InstalledPackages

> main :: IO ()
> main = do
>   args <- getArgs
>   pkgs <- readPackages
>   recursiveGetSources pkgs args [] >>= putStrLn . groom

