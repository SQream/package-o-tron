
> import Development.Pot.Modules
> import System.Environment

> main :: IO ()
> main = do
>   args <- getArgs
>   mi <- modulesInfo args
>   putStrLn $ showmi mi
