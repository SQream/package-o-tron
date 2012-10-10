

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
> --import Data.Attoparsec.Combinator
> import Control.Applicative
> import Debug.Trace
> import Prelude hiding (takeWhile)
> --import Data.Maybe
> import System.FilePath
> import Distribution.Pot.Types

attempt to write a parser which can quickly read the module name and
imports from a haskell file


features:
does normal .hs and .lhs birdfeet style
deals with block comments
stops parsing early

> filterBirdfeet :: LT.Text -> LT.Text
> filterBirdfeet = LT.unlines
>                  . map (LT.drop 2)
>                  . filter (\l -> not (LT.null l) && LT.head l == '>')
>                  . LT.lines

> data SourceType = Hs | Lhs

> sourceTypeOf :: FilePath -> SourceType
> sourceTypeOf fn = case takeExtension fn of
>     ".hs" -> Hs
>     ".lhs" -> Lhs
>     e -> error $ "unknown extension: " ++ e

> parseSource :: SourceType -> LT.Text -> Either String SourceSyntaxInfo
> parseSource Lhs t = parseSource Hs $ filterBirdfeet t
> parseSource Hs t = eitherResult $ parse sourceFile t

> sourceFile :: Parser SourceSyntaxInfo
> sourceFile = do
>   skipLines
>   mn <- option Nothing (Just <$> modulep)
>   is <- many importp
>   return SSI {ssiModuleName = mn
>              ,ssiImports = is}

assume there are no block comments on a module or import line
between the start of the line and the end of the module name

> modulep :: Parser T.Text
> modulep = do
>     void $ string "module"
>     sws
>     mn <- dottedIden
>     dropTillEndOfLine
>     skipLines
>     return mn

> importp :: Parser T.Text
> importp = do
>     void $ string "import"
>     sws
>     option () (void (string "qualified") >> sws)
>     mn <- dottedIden
>     dropTillEndOfLine
>     skipLines
>     return mn

> dottedIden :: Parser T.Text
> dottedIden = takeWhile1 (\x -> isAlpha x || x == '.')

> -- skip any whitespace except newline
> sws :: Parser ()
> sws = skipWhile $ \x -> isSpace x && not (x == '\n')

> -- skip lines which don't start with a character
> -- so we can get to the next import or declaration
> -- skips over multiline nested block comments and line comments
> skipLines :: Parser ()
> skipLines =
>     choice [do
>             x <- peekChar
>             guard $ maybe True (not . isAlpha) x
>             dropTillEndOfLine
>             skipLines
>            ,return ()]

> -- skip till end of current line, but also handles
> -- multiline block comments by skipping to the next line after the
> -- comment ends
> dropTillEndOfLine :: Parser ()
> dropTillEndOfLine = do
>     void $ manyTill anyChar (choice
>                              [blockComment >> dropTillEndOfLine
>                              ,void $ char '\n'])

> blockComment :: Parser ()
> blockComment = do
>     void $ string "{-"
>     -- nested commented not working, not sure why
>     voidTrace "bc" $ manyTill anyChar (choice [blockComment
>                                               ,void $ string "-}"])

> voidTrace :: Show a => String -> Parser a -> Parser ()
> voidTrace m p = do
>   x <- p
>   trace (m ++ ": " ++ show x) $ return ()

> {-traceSkipWhile :: String -> (Char -> Bool)
>                -> Parser ()
> traceSkipWhile m p = do
>   x <- takeWhile p
>   trace (m ++ ": " ++ T.unpack x) $ return ()-}
