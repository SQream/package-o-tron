
Dumps the parsed file and package dependency information for a set of
files


> import Distribution.Pot.Modules
> import System.Environment

> main :: IO ()
> main = do
>   args <- getArgs
>   mi <- modulesInfo args
>   putStrLn $ showmi mi
