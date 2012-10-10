
Used for testing: just dump the parsed installed package list to check
the ghc-pkg dump parser.

> import Distribution.Pot.InstalledPackages
> import Data.List
> --import Distribution.InstalledPackageInfo

> main :: IO ()
> main = do
>   {-x <- readPackages
>   putStrLn $ intercalate "\n\n" $ map s x
>   where
>     s (nm,ms) = nm ++ "\n------\n"
>                 ++ intercalate "\n" ms-}
>     x <- readPackages
>     putStrLn $ intercalate "\n\n" $ map s x
>     where
>       s = show
>       {-s ipi = concat
>               [show $ installedPackageId ipi
>               ,"\n"
>               ,unwords $ map show $ exposedModules ipi
>               ,"\n\n"
>               ,unwords $ map show $ depends ipi
>               ]-}
