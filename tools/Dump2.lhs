
> import Distribution.Pot.RecursiveGetSources
> import System.Environment
> --import Control.Monad
> import Text.Groom
> --import Distribution.Pot.Types

> main :: IO ()
> main = do
>   args <- getArgs
>   recursiveGetSources args ["src"] >>= putStrLn . groom

