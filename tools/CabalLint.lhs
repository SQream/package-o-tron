
> {-# LANGUAGE TupleSections #-}
> import System.Environment
> import qualified Data.Text as T
> import Distribution.Pot.CabalLint

> data Opts =
>     Opts
>     {hidePackages :: [T.Text]
>     ,cabalFile :: FilePath
>     } deriving Show

> main :: IO ()
> main = do
>   opts <- parseArgs `fmap` getArgs
>   li <- cabalLint (cabalFile opts) (hidePackages opts)
>   putStrLn $ unlines $
>            showItems "unmatchedImports" sft (unmatchedImports li)
>            ++ showItems "ambiguousImports" sft (ambiguousImports li)
>            ++ showItems "extra other modules" sd (extraOtherModules li)
>            ++ showItems "missing other modules" sd (missingOtherModules li)
>            ++ showItems "extra build deps" sd (extraBuildDeps li)
>            ++ showItems "missing build deps" sd (missingBuildDeps li)
>            ++ showItems "won't accept packages" sv (wontAccept li)
>   return ()
>   where
>     sft (f,t) = f ++ ": " ++ T.unpack t
>     sd ((st,t),ds) = T.unpack t ++ "(" ++ show st ++ "): " ++ unlines (map T.unpack ds)
>     sv (p,v) = p ++ "-" ++ v
>     showItems :: String -> (a -> String) -> [a] -> [String]
>     showItems m r as =
>         if null as
>         then []
>         else m : map r as

> parseArgs :: [String] -> Opts
> parseArgs = f [] Nothing
>   where
>     f ps c ("--hide-package":p:xs) =
>         f (p:ps) c xs
>     f _ps _c (["--hide-package"]) =
>         error "no package name after --hide-package"
>     f ps Nothing (x:xs) =
>         f ps (Just x) xs
>     f _ps (Just _) (_:_) =
>         error "please pass only one cabal file"
>     f ps (Just c) [] =
>         Opts (map T.pack ps) c
>     f _ps Nothing [] = error "please pass cabal file"
