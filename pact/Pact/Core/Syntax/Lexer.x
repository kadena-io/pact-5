{
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}


module Pact.Core.Syntax.Lexer(lexer, runLexerIO) where

import Control.Monad.State.Strict
import Control.Exception(throwIO)
import Data.Char(isSpace)
import Data.Text(Text)
import qualified Data.Text as T

import Pact.Core.Info
import Pact.Core.Errors
import Pact.Core.Syntax.LexUtils

}
%encoding "latin1"

$lower = [ a-z ]
$digit = [ 0-9 ]
$alpha = [a-zA-Z]
$psymbol = [\%\#\+\-\_\&\$\@\<\>\=\^\?\*\!\|\/\~]
@ident = [$alpha $psymbol][$alpha $digit $psymbol]*
@integer = [\-]?[$digit]+
@singletick = [\'][$alpha][$alpha $digit \- \_]*
@comment = [\;][.]*[\n]?
@tc = expect\-typechecks
@tcfail = expect\-typecheck\-failure
@steprb = step\-with\-rollback
@withcap = with\-capability
@cruserguard = create\-user\-guard
@enforce = enforce
@enforceOne = enforce\-one



tokens :-
    @comment;
    $white+;
    -- Keywords
    let\*        { token TokenLet }
    let          { token TokenLet }
    if           { token TokenIf }
    defun        { token TokenDefun }
    defcap       { token TokenDefCap }
    defconst     { token TokenDefConst }
    defschema    { token TokenDefSchema }
    deftable     { token TokenDefTable }
    defpact      { token TokenDefPact }
    interface    { token TokenInterface }
    module       { token TokenModule }
    bless        { token TokenBless }
    implements   { token TokenImplements }
    use          { token TokenImport }
    true         { token TokenTrue }
    false        { token TokenFalse }
    lambda       { token TokenLambda }

    and          { token TokenAnd }
    or           { token TokenOr }
    load         { token TokenLoad }
    \@doc        { token TokenDocAnn }
    \@model      { token TokenModelAnn}
    \@event      { token TokenEventAnn }
    \@managed    { token TokenManagedAnn}
    @steprb      { token TokenStepWithRollback}
    @enforce     { token TokenEnforce }
    @enforceOne  { token TokenEnforceOne }
    step         { token TokenStep }
    @withcap     { token TokenWithCapability }
    @cruserguard { token TokenCreateUserGuard }
    try          { token TokenTry }
    progn        { token TokenBlockIntro }
    suspend      { token TokenSuspend }

    @integer     { emit TokenNumber }

    @singletick  { emit TokenSingleTick }
    \(           { token TokenOpenParens }
    \)           { token TokenCloseParens }
    \{           { token TokenOpenBrace }
    \}           { token TokenCloseBrace }
    \[           { token TokenOpenBracket }
    \]           { token TokenCloseBracket }
    \,           { token TokenComma }
    \.           { token TokenDot }
    \:\=         { token TokenBindAssign }
    \:\:         { token TokenDynAcc }
    \:           { token TokenColon }
    \"           { stringLiteral }
    @ident       { emit TokenIdent }
{

-- TODO: non-horrible errors
scan :: LexerM PosToken
scan = do
  input@(AlexInput sLine sCol _ txt) <- get
  case alexScan input 0 of
    AlexEOF -> pure (PosToken TokenEOF (SpanInfo sLine sCol (sLine+1) 0))

    AlexError (AlexInput eLine eCol  _last inp) ->
      let li = SpanInfo sLine sCol eLine eCol
      in case T.uncons inp of
        Just (h, _) ->
          throwLexerError (LexicalError h _last) li
        Nothing -> throwLexerError (OutOfInputError _last) li
    AlexSkip input' _ -> do
      put input'
      scan
    AlexToken input'@(AlexInput eLine eCol _ _) tokl action -> do
      put input'
      let
        span' = SpanInfo sLine sCol eLine eCol
        t = T.take (fromIntegral tokl) txt
      action t span'

stringLiteral :: Text -> SpanInfo -> LexerM PosToken
stringLiteral _ info = do
  inp <- get
  body <- loop [] inp
  pure (PosToken (TokenString (T.pack body)) info)
  where
  loop acc inp =
    case lexerGetChar inp of
      Just (c, rest) ->
        handleChar acc c rest
      Nothing -> throwLexerError' $ StringLiteralError "did not close string literal"
  handleChar acc c rest
    | c == '\\' = escape acc rest
    | c == '\n' = throwLexerError' $ StringLiteralError "newline in string literal"
    | c == '\r' = throwLexerError' $ StringLiteralError "carriage return in string literal"
    | c == '\"' = reverse acc <$ put rest
    | otherwise = loop (c:acc) rest
  multiLine acc inp =
    case lexerGetChar inp of
      Just (c, rest)
        | isSpace c -> multiLine acc rest
        | c == '\\' -> loop acc rest
        | otherwise -> throwLexerError' $ StringLiteralError "Invalid multiline string"
      Nothing -> throwLexerError' $ StringLiteralError "did not close string literal"
  escape acc inp =
    case lexerGetChar inp of
      Just (c, rest)
        | isSpace c -> multiLine acc rest
        | c == 'n' -> loop ('\n':acc) rest
        | c == 't' -> loop ('\t':acc) rest
        | c == '\\' -> loop ('\\':acc) rest
        | c == '\"' -> loop ('\"':acc) rest
        | c == '\'' -> loop ('\'':acc) rest
        | c == 'r' -> throwLexerError' $ StringLiteralError "carriage return is not supported in strings literals"
        | otherwise -> throwLexerError' $ StringLiteralError "Invalid escape sequence"
      Nothing -> throwLexerError' $ StringLiteralError "did not close string literal"

-- A colon _may_ indicate the start of a block,
-- so we emit the token and push the start code.
scanTokens :: LexerM [PosToken]
scanTokens = scan' []
  where
  scan' acc =
    scan >>= \case
      PosToken TokenEOF _ -> pure (reverse acc)
      tok -> scan' (tok:acc)

lexer :: Text -> Either PactErrorI [PosToken]
lexer bs = runLexerT scanTokens bs

runLexerIO :: Text -> IO [PosToken]
runLexerIO bs = either throwIO pure (lexer bs)
}
