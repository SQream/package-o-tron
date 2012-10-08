
Dumps the parsed file and package dependency information for a set of
files


> import Distribution.Pot.Modules
> import System.Environment
> import Data.List
> --import Text.Groom

> main :: IO ()
> main = do
>   args <- getArgs
>   mis <- modulesInfo args
>   putStrLn $ intercalate "\n\n" $ map ppMI mis
