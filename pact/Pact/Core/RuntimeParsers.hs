module Pact.Core.RuntimeParsers where

import Control.Applicative
import Data.Attoparsec.Text
import Text.Parser.Char(oneOf)
import Data.Text(Text)
import Text.Parser.Token
import Text.Parser.Token.Highlight
import qualified Data.HashSet as HS


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
