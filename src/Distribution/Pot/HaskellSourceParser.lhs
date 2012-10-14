
> -- | Parser to efficiently read the module name and
> -- imports from a haskell file.
> -- Does normal .hs and .lhs birdfeet style.
> -- Deals with most (nested) block comments, apart from ones between the
> -- start of the line with 'module' or 'import' text in a decl and the
> -- end of the module name on that line.

> {-# LANGUAGE OverloadedStrings #-}
> module Distribution.Pot.HaskellSourceParser
>    (SourceType(..)
>    ,sourceTypeOf
>    ,parseSource) where

> import qualified Data.Text.Lazy as LT
> import Data.Attoparsec.Text.Lazy
> import qualified Data.Text as T
> import Data.Char
> import Control.Monad
> import Control.Applicative
> import Prelude hiding (takeWhile)
> import System.FilePath
> import Distribution.Pot.Types

> -- | is the source regular haskell or birdfeet style literate haskell
> data SourceType = Hs | Lhs

> -- | get the source type from the extension in the filename
> sourceTypeOf :: FilePath -> SourceType
> sourceTypeOf fn = case takeExtension fn of
>     ".hs" -> Hs
>     ".lhs" -> Lhs
>     e -> error $ "unknown extension: " ++ e

> -- | parse some haskell source
> parseSource :: SourceType -> LT.Text -> Either String SourceImports
> parseSource Lhs t = parseSource Hs $ filterBirdfeet t
> parseSource Hs t = eitherResult $ parse sourceFile t

> sourceFile :: Parser SourceImports
> sourceFile = do
>   skipLines
>   mn <- option Nothing (Just <$> modulep)
>   is <- many importp
>   return SourceImports {siModuleName = mn
>                        ,siImports = "Prelude" : is}

assume there are no block comments on a module or import line
between the start of the line and the end of the module name

> modulep :: Parser T.Text
> modulep = do
>     void $ string "module"
>     sws
>     mn <- dottedIden
>     dropTillEndOfLineAndSkipLines
>     return mn

> importp :: Parser T.Text
> importp = do
>     void $ string "import"
>     sws
>     option () (void (string "qualified") >> sws)
>     mn <- dottedIden
>     dropTillEndOfLineAndSkipLines
>     return mn

> dottedIden :: Parser T.Text
> dottedIden = do
>   x <- satisfy isAlpha
>   y <- takeWhile (\z -> isAlphaNum z || z == '.')
>   return $ T.cons x y

> -- skip any whitespace except newline
> sws :: Parser ()
> sws = skipWhile $ \x -> isSpace x && (x /= '\n')

> -- skip lines which don't start with a character
> -- so we can get to the next import or declaration
> -- skips over multiline nested block comments and line comments
> skipLines :: Parser ()
> skipLines =
>     choice [do
>             x <- peekChar
>             guard $ maybe True (not . isAlpha) x
>             dropTillEndOfLineAndSkipLines
>            ,return ()]

> -- skip till end of current line, but also handles
> -- multiline block comments by skipping to the next line after the
> -- comment ends
> dropTillEndOfLine :: Parser ()
> dropTillEndOfLine = do
>     void $ manyTill anyChar (choice
>                              [blockComment >> dropTillEndOfLine
>                              ,void $ char '\n'])
>     skipLines

> dropTillEndOfLineAndSkipLines :: Parser ()
> dropTillEndOfLineAndSkipLines = dropTillEndOfLine >> skipLines

> blockComment :: Parser ()
> blockComment = do
>     void $ string "{-"
>     let suffix :: Parser ()
>         suffix = void $ manyTill anyChar (choice [do
>                                                   blockComment
>                                                   suffix
>                                                  ,void $ string "-}"])
>     suffix

> filterBirdfeet :: LT.Text -> LT.Text
> filterBirdfeet = LT.unlines
>                  . map (LT.drop 2)
>                  . filter (\l -> not (LT.null l) && LT.head l == '>')
>                  . LT.lines


> {-voidTrace :: Show a => String -> Parser a -> Parser ()
> voidTrace _m p = do
>   _x <- p
>   --trace (m ++ ": " ++ show x) $ return ()
>   return ()-}

> {-traceSkipWhile :: String -> (Char -> Bool)
>                -> Parser ()
> traceSkipWhile m p = do
>   x <- takeWhile p
>   trace (m ++ ": " ++ T.unpack x) $ return ()-}
