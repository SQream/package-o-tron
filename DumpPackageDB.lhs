


> import Development.Pot.Packages
> import Data.List

> main :: IO ()
> main = do
>   x <- readPackages
>   putStrLn $ intercalate "\n\n" $ map s x
>   where
>     s (nm,ms) = nm ++ "\n------\n"
>                 ++ intercalate "\n" ms
