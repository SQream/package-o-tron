

> import Distribution.Pot.HaskellSourceParser

> import qualified Data.Text.Lazy.IO as T
> import Text.Groom

test the haskell source parser for nested block comments

> {- first  {- nested -} middle
> import qualified Data.Text.Lazy as T
> end -}
> --import System.FilePath

> import System.Environment

> main :: IO ()
> main = do
>   [fn] <- getArgs
>   f <- T.readFile fn
>   putStrLn $ groom $ parseSource (sourceTypeOf fn) f
