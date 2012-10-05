
input a set of folders

finds all the haskell source in these folders and follows all the
imports of other source in these folders, and outputs the complete
list of directly referenced packages

> import Development.Pot.Modules
> import System.Environment
> import Control.Arrow
> import Control.Applicative
> import Data.List

> main :: IO ()
> main = do
>   args <- getArgs
>   mi <- map snd <$> modulesInfo args
>   let ps = sort $ nub $ concatMap miPackages mi
>   putStrLn $ intercalate "\n" ps
