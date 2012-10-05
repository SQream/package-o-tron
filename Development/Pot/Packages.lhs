
> module Development.Pot.Packages (readPackages) where

> import Data.List
> import System.Process
> import Text.Groom

returns a map from package name to the modules in that package

> readPackages :: IO [(String,[String])]
> readPackages = do
>   inf <- readProcess "ghc-pkg" ["dump"] ""
>   return $ kludgePackages $ parsePackages [] $ lines inf
>   where
>     parsePackages acc (f:v) | "name: " `isPrefixOf` f =
>         parseModules acc (drop (length "name: ") f) v
>     parsePackages acc (_:v) = parsePackages acc v
>     parsePackages acc [] = acc
>     parseModules acc nm (f:v) | "exposed-modules:" `isPrefixOf` f =
>       parseMoreModules acc nm [drop (length "exposed-modules:") f] v
>     parseModules acc nm (_:v) = parseModules acc nm v
>     parseModules _ nm [] = error $ "didn't find exposed-modules for package " ++ nm
>     parseMoreModules acc nm macc ((' ':f):v) = parseMoreModules acc nm (f:macc) v
>     parseMoreModules acc nm macc v =
>       let ms = (nm, words $ intercalate " " macc)
>       in parsePackages (ms:acc) v

> kludgePackages :: [(String,[String])] -> [(String,[String])]
>     -- lots of modules appear in base, haskell98, haskell2010
>     -- just use base for now.
> kludgePackages = filter ((`notElem` ["haskell98"
>                                     ,"haskell2010"]) . fst)


> _s :: [(String,[String])] -> String
> _s = intercalate "\n\n" . map (\(n,ms) -> n ++ "\n" ++ groom ms ++ "\n")
