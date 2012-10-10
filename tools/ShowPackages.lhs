
input a set of folders

finds all the haskell source in these folders and follows all the
imports of other source in these folders, and outputs the complete
list of directly referenced packages

TODO: option to include all the indirectly depended packages
to output package versions as well
to exclude system packages
to collect the tarballs for all these packages

> import Distribution.Pot.RecursiveGetSources
> import System.Environment
> --import Control.Applicative
> import Data.List
> import Distribution.Pot.InstalledPackages
> import Distribution.Pot.DeepDependencies
> import Distribution.Pot.Types
> --import qualified Data.Text as T
> --import Data.Either
> --import Text.Groom

> main :: IO ()
> main = do
>   args <- getArgs
>   pkgs <- readPackages
>   srcs <- recursiveGetSources pkgs args []
>   let asrcs = deepDependencies pkgs srcs
>   putStrLn $ intercalate "\n\n" $ map {-(\x -> groom x
>                                              ++ -}ppEssi {-x)-} asrcs
