

> import Distribution.Pot.HaskellSourceParser

> import qualified Data.Text.Lazy.IO as T
> import Text.Groom
> import System.Environment
> {- first  {- ini -} middle
>   import qualified Data.Text.Lazy as T
>   end -}
> --import System.FilePath

> main :: IO ()
> main = do
>   [fn] <- getArgs
>   f <- T.readFile fn
>   putStrLn $ groom $ parseSource (sourceTypeOf fn) f
