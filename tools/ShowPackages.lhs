
input a set of folders

finds all the haskell source in these folders and follows all the
imports of other source in these folders, and outputs the complete
list of directly referenced packages

TODO: option to include all the indirectly depended packages
to output package versions as well
to exclude system packages
to collect the tarballs for all these packages

> import Distribution.Pot.Modules
> import System.Environment
> --import Control.Applicative
> import Data.List

> main :: IO ()
> main = do
>   args <- getArgs
>   mi <- modulesInfo args
>   let ps = sort $ nub $ concatMap miDirectDeepPackages mi
>   putStrLn $ intercalate "\n" ps
