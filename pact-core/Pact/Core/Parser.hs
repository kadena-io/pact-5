-- | 

module Pact.Core.Parser
  ( nameMatcher
  , keysetNameParser
  , moduleNameParser
  , ident'
  , style
  , fullyQualNameParser
  ) where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Text(Text)
import Text.Parser.Char(oneOf)
import Text.Parser.Token
import Text.Parser.Token.Highlight
import qualified Data.HashSet as HS
import qualified Data.Text as T

import qualified Data.ByteString.Short as SB
import qualified Data.ByteString.Char8 as BS

import Pact.Core.Guards
import Pact.Core.Names
import Pact.Core.Hash

nameMatcher :: Parser Text
nameMatcher = asMatcher $ qualifiedNameMatcher
                      <|> bareNameMatcher
  where
    bareNameMatcher :: Parser ()
    bareNameMatcher = void $ ident' style
    qualifiedNameMatcher :: Parser ()
    qualifiedNameMatcher = do
      void $ ident' style
      void $ dot *> ident' style
      void $ optional (dot *> ident' style)
    asMatcher :: Parser a -> Parser Text
    asMatcher = fmap fst . match

keysetNameParser :: Parser KeySetName
keysetNameParser = qualified <|> withoutNs
  where
    qualified = do
      ns <- NamespaceName <$> ident style
      kn <- dot *> ident style
      pure $ KeySetName kn (Just ns) -- TODO: @Georg, is this correct?
    withoutNs = do
      t <- takeText
      guard $ not $ T.null t
      pure $ KeySetName t Nothing

moduleNameParser :: Parser ModuleName
moduleNameParser = do
  a <- ident style
  b <- optional (dot *> ident style)
  case b of
    Nothing -> pure $ ModuleName a Nothing
    Just b' -> pure $ ModuleName b' (Just $ NamespaceName a)

-- type-specialized version of `ident`
-- to avoid defaulting warnings on the `IsString` constraint
ident' :: IdentifierStyle Parser -> Parser Text
ident' = ident

style :: IdentifierStyle Parser
style = IdentifierStyle "atom"
        (letter <|> symbols)
        (letter <|> digit <|> symbols)
        (HS.fromList ["true", "false"])
        Symbol
        ReservedIdentifier
  where
    symbols :: Parser Char
    symbols = oneOf "%#+-_&$@<>=^?*!|/~"


fullyQualNameParser :: Parser FullyQualifiedName
fullyQualNameParser = do
  qualifier <- ident style
  mname <- dot *> ident style
  oname <- optional (dot *> ident style)
  h <- dot *> (between' (char '{') (char '}') $ some (alphaNum <|> char '-' <|> char '_'))
  hash' <- case decodeBase64UrlUnpadded (BS.pack h) of
    Right hash' -> pure $ SB.toShort hash'
    Left _ -> fail "invalid hash encoding"
  case oname of
    Just nn ->
      pure (FullyQualifiedName (ModuleName mname (Just $ NamespaceName qualifier)) nn (ModuleHash (Hash hash')))
    Nothing ->
      pure (FullyQualifiedName (ModuleName qualifier Nothing) mname (ModuleHash (Hash hash')))

between' :: Parser a -> Parser b -> Parser c -> Parser c
between' open close p = open *> p <* close

alphaNum :: Parser Char
alphaNum = letter <|> digit
