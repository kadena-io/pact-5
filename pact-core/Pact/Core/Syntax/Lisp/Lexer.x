{
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}


module Pact.Core.Syntax.Lisp.Lexer(lexer, runLexerIO) where

import Control.Monad.State.Strict
import Control.Exception(throwIO)
import Data.Char(isSpace)
import Data.Text(Text)
import Data.ByteString(ByteString)
import Data.ByteString.Internal(w2c)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Pact.Core.Info
import Pact.Core.Errors
import Pact.Core.Syntax.Lisp.LexUtils

}
%encoding "latin1"

$lower = [ a-z ]
$digit = [ 0-9 ]
$alpha = [a-zA-Z]
$special = [\.\;\,\$\|\*\+\?\#\~\-\{\}\(\)\[\]\^\/]
@ident = [$alpha][$alpha $digit \-]*
@integer = [\-]?[$digit]+
@singletick = [\'][$alpha][$alpha $digit \-]*
@comment = [\;][.]*[\n]
@tc = expect\-typechecks
@tcfail = expect\-typecheck\-failure
@steprb = step\-with\-rollback


tokens :-
    @comment;
    $white+;
    -- Keywords
    let          { token TokenLet }
    let\*        { token TokenLet }
    if           { token TokenIf }
    defun        { token TokenDefun }
    defcap       { token TokenDefCap }
    defconst     { token TokenDefConst }
    defschema    { token TokenDefSchema }
    deftable     { token TokenDefTable }
    defcap       { token TokenDefCap }
    defpact      { token TokenDefPact }
    interface    { token TokenInterface }
    module       { token TokenModule }
    bless        { token TokenBless }
    implements   { token TokenImplements }
    use          { token TokenImport }
    true         { token TokenTrue }
    false        { token TokenFalse }
    keyGov       { token TokenKeyGov }
    capGov       { token TokenCapGov }
    bool         { token TokenTyBool }
    lambda       { token TokenLambda }
    integer      { token TokenTyInteger }
    bool         { token TokenTyBool }
    table        { token TokenTyTable }
    decimal      { token TokenTyDecimal }
    string       { token TokenTyString }
    unit         { token TokenTyUnit }
    and          { token TokenAnd }
    or           { token TokenOr }
    load         { token TokenLoad }
    \@doc        { token TokenDocAnn }
    \@model      { token TokenModelAnn}
    @steprb      { token TokenStepWithRollback}
    step         { token TokenStep }
    @tc          { token TokenTypechecks }
    @tcfail      { token TokenTypecheckFailure }
    -- at           { token TokenObjAccess }
    -- remove       { token TokenObjRemove }
    try          { token TokenTry }
    error        { token TokenError }
    progn        { token TokenBlockIntro }
    suspend      { token TokenSuspend }

    @integer     { emit TokenNumber }
    @ident       { emit TokenIdent }
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
    \=           { token TokenEq }
    \!\=         { token TokenNeq }
    \>\=         { token TokenGEQ }
    \>           { token TokenGT }
    \<\=         { token TokenLEQ }
    \<           { token TokenLT }
    \+           { token TokenPlus }
    \-           { token TokenMinus }
    \*           { token TokenMult }
    \/           { token TokenDiv }
    \&           { token TokenBitAnd }
    \|           { token TokenBitOr }
    \~           { token TokenBitComplement }
    \"           { stringLiteral }
    \-\>         { token TokenTyArrow }
    \^           { token TokenPow }

{
-- TODO: non-horrible errors
scan :: LexerM PosToken
scan = do
  input@(AlexInput _ _ _ bs) <- get
  case alexScan input 0 of
    AlexEOF -> withLineInfo TokenEOF
    AlexError (AlexInput line col _last inp) ->
      let li = LineInfo line col 1
      in case B.uncons inp of
        Just (h, _) ->
          throwLexerError (LexicalError (w2c h) _last) li
        Nothing -> throwLexerError (OutOfInputError _last) li
    AlexSkip input' _ -> do
      put input'
      scan
    AlexToken input' tokl action -> do
      put input'
      let t = T.decodeLatin1 (B.take (fromIntegral tokl) bs)
      action t

stringLiteral :: Text -> LexerM PosToken
stringLiteral _ = do
  inp <- get
  info <- getLineInfo
  body <- loop [] inp
  pure (PosToken (TokenString (T.pack body)) info)
  where
  loop acc inp =
    case alexGetByte inp of
      Just (c, rest) ->
        handleChar acc (w2c c) rest
      Nothing -> throwLexerError' $ StringLiteralError "did not close string literal"
  handleChar acc c rest
    | c == '\\' = escape acc rest
    | c == '\n' = throwLexerError' $ StringLiteralError "newline in string literal"
    | c == '\r' = throwLexerError' $ StringLiteralError "carriage return in string literal"
    | c == '\"' = reverse acc <$ put rest
    | otherwise = loop (c:acc) rest
  multiLine acc inp =
    case alexGetByte inp of
      Just (w2c -> c, rest)
        | isSpace c -> multiLine acc rest
        | c == '\\' -> loop acc rest
        | otherwise -> throwLexerError' $ StringLiteralError "Invalid multiline string"
      Nothing -> throwLexerError' $ StringLiteralError "did not close string literal"
  escape acc inp =
    case alexGetByte inp of
      Just (w2c -> c, rest)
        | c == '\n' -> multiLine acc rest
        | c == 'n' -> loop ('\n':acc) rest
        | c == 't' -> loop ('\t':acc) rest
        | c == '\\' -> loop ('\\':acc) rest
        | c == '\"' -> loop ('\"':acc) rest
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

lexer :: ByteString -> Either PactErrorI [PosToken]
lexer bs = runLexerT scanTokens bs

runLexerIO :: ByteString -> IO [PosToken]
runLexerIO bs = either throwIO pure (lexer bs)
}
