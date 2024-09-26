{
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Pact.Core.Syntax.Parser where

import Control.Lens(preview, view, _head, _3)
import Control.Monad(when)
import Control.Monad.Except

import Data.Decimal(DecimalRaw(..))
import Data.Char(digitToInt)
import Data.Text(Text)
import Data.List.NonEmpty(NonEmpty(..))
import Data.Either(lefts, rights)
import Data.Maybe(catMaybes)

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.List.NonEmpty as NE

import Pact.Core.Names
import Pact.Core.Info
import Pact.Core.Literal
import Pact.Core.Builtin
import Pact.Core.Type(PrimType(..))
import Pact.Core.Guards
import Pact.Core.Errors
import Pact.Core.Syntax.ParseTree
import Pact.Core.Syntax.LexUtils


}
%name parseExpr Expr
%name parseModule Module
%name parseReplProgram ReplProgram
%name parseProgram Program

%tokentype { PosToken }
%monad { ParserT }

%errorhandlertype explist
%error { parseError }


%token
  let        { PosToken TokenLet _ }
  letstar    { PosToken TokenLetStar _ }
  lam        { PosToken TokenLambda _ }
  module     { PosToken TokenModule _ }
  interface  { PosToken TokenInterface _ }
  import     { PosToken TokenImport _ }
  defun      { PosToken TokenDefun _ }
  defcap     { PosToken TokenDefCap _ }
  defconst   { PosToken TokenDefConst _ }
  defschema  { PosToken TokenDefSchema _ }
  deftable   { PosToken TokenDefTable _ }
  defpact    { PosToken TokenDefPact _ }
  bless      { PosToken TokenBless _}
  implements { PosToken TokenImplements _ }
  true       { PosToken TokenTrue _ }
  false      { PosToken TokenFalse _ }
  docAnn     { PosToken TokenDocAnn _ }
  modelAnn   { PosToken TokenModelAnn _ }
  eventAnn   { PosToken TokenEventAnn _ }
  managedAnn { PosToken TokenManagedAnn _ }
  step       { PosToken TokenStep _ }
  steprb     { PosToken TokenStepWithRollback _ }
  '{'        { PosToken TokenOpenBrace _ }
  '}'        { PosToken TokenCloseBrace _ }
  '('        { PosToken TokenOpenParens _ }
  ')'        { PosToken TokenCloseParens _ }
  '['        { PosToken TokenOpenBracket _ }
  ']'        { PosToken TokenCloseBracket _ }
  ','        { PosToken TokenComma _ }
  '::'       { PosToken TokenDynAcc _ }
  ':'        { PosToken TokenColon _ }
  ':='       { PosToken TokenBindAssign _ }
  '.'        { PosToken TokenDot _ }
  IDENT      { PosToken (TokenIdent _) _ }
  NUM        { PosToken (TokenNumber _) _ }
  STR        { PosToken (TokenString _) _ }
  TICK       { PosToken (TokenSingleTick _) _}

%nonassoc ARG

%%

Program :: { [ParsedTopLevel] }
  : ProgramList { reverse $1 }

ProgramList :: { [ParsedTopLevel] }
  : ProgramList TopLevel { $2:$1 }
  | {- empty -} { [] }

ReplProgram :: { [ReplTopLevel SpanInfo] }
  : ReplProgramList { reverse $1 }

ReplProgramList :: { [ReplTopLevel SpanInfo] }
  : ReplProgramList ReplTopLevel { $2:$1 }
  | {- empty -} { [] }

TopLevel :: { ParsedTopLevel }
  : Module { TLModule $1 }
  | Interface { TLInterface $1 }
  | Expr { TLTerm $1 }
  | Use { uncurry TLUse $1 }

ReplTopLevel :: { ReplTopLevel SpanInfo }
  : TopLevel { RTLTopLevel $1 }
  | '(' Defun ')' { RTLDefun ($2 (combineSpan (_ptInfo $1) (_ptInfo $3))) }
  | '(' DefConst ')' { RTLDefConst ($2 (combineSpan (_ptInfo $1) (_ptInfo $3))) }

Governance :: { Governance ParsedName }
  : StringRaw { KeyGov (KeySetName $1 Nothing) }
  | IDENT { CapGov (FQParsed (BN (BareName (getIdent $1)))) }

StringRaw :: { Text }
 : STR  { getStr $1 }
 | TICK { getTick $1 }

Module :: { ParsedModule }
  : '(' module IDENT Governance MDocOrModel ExtOrDefs ')'
    { Module (ModuleName (getIdent $3) Nothing) $4 (reverse (rights $6)) (NE.fromList (reverse (lefts $6))) $5
      (combineSpan (_ptInfo $1) (_ptInfo $7)) }

Interface :: { ParsedInterface }
  : '(' interface IDENT MDocOrModel ImportOrIfDef ')'
    { Interface (ModuleName (getIdent $3) Nothing) (reverse (lefts $5)) (reverse (rights $5)) $4
      (combineSpan (_ptInfo $1) (_ptInfo $2)) }

Ext :: { ExtDecl }
  : Use { ExtImport (fst $1)  }
  | '(' implements ModQual ')' { ExtImplements (mkModName $3) }
  | '(' bless StringRaw ')' { ExtBless $3 }

Use :: { (Import, SpanInfo) }
  : '(' import ModQual ImportList ')' {  (Import (mkModName $3) Nothing $4, combineSpan (_ptInfo $1) (_ptInfo $5))  }
  | '(' import ModQual STR ImportList ')' {  (Import (mkModName $3) (Just (getStr $4)) $5, combineSpan (_ptInfo $1) (_ptInfo $6))  }


ExtOrDefs :: { [Either (Def SpanInfo) ExtDecl] }
  : ExtOrDefs Def { (Left $2):$1 }
  | ExtOrDefs Ext { (Right $2) : $1 }
  | Def { [Left $1] }
  | Ext { [Right $1] }

Def :: { ParsedDef }
  : '(' Defun ')' { Dfun ($2 (combineSpan (_ptInfo $1) (_ptInfo $3))) }
  | '(' DefConst ')' { DConst ($2 (combineSpan (_ptInfo $1) (_ptInfo $3))) }
  | '(' Defcap ')'  { DCap ($2 (combineSpan (_ptInfo $1) (_ptInfo $3)))  }
  | '(' Defschema ')' { DSchema ($2 (combineSpan (_ptInfo $1) (_ptInfo $3)))  }
  | '(' Deftable ')' { DTable ($2 (combineSpan (_ptInfo $1) (_ptInfo $3))) }
  | '(' DefPact ')' { DPact ($2 (combineSpan (_ptInfo $1) (_ptInfo $3))) }

ImportOrIfDef :: { [Either ParsedIfDef Import] }
  : ImportOrIfDef IfDef { (Left $2):$1 }
  | ImportOrIfDef Use { (Right (fst $2)) : $1 }
  | IfDef { [Left $1] }
  | Use { [Right (fst $1)] }

IfDef :: { ParsedIfDef }
  : '(' IfDefun ')' { IfDfun ($2 (combineSpan (_ptInfo $1) (_ptInfo $3))) }
  | '(' DefConst ')' { IfDConst ($2 (combineSpan (_ptInfo $1) (_ptInfo $3))) }
  | '(' IfDefCap ')'{ IfDCap ($2 (combineSpan (_ptInfo $1) (_ptInfo $3))) }
  | '(' Defschema ')' { IfDSchema ($2 (combineSpan (_ptInfo $1) (_ptInfo $3))) }
  | '(' IfDefPact ')' { IfDPact ($2 (combineSpan (_ptInfo $1) (_ptInfo $3))) }

-- ident = $2,
IfDefun :: { SpanInfo -> IfDefun SpanInfo }
  : defun IDENT MTypeAnn '(' MArgs ')' MDocOrModel
    { IfDefun (MArg (getIdent $2) $3 (_ptInfo $2)) (reverse $5) $7 }

IfDefCap :: { SpanInfo -> IfDefCap SpanInfo }
  : defcap IDENT MTypeAnn'(' MArgs ')' MDocOrModel MDCapMeta
    { IfDefCap (MArg (getIdent $2) $3 (_ptInfo $2)) (reverse $5) $7 $8 }

IfDefPact :: { SpanInfo -> IfDefPact SpanInfo }
  : defpact IDENT MTypeAnn '(' MArgs ')' MDocOrModel
    { IfDefPact (MArg (getIdent $2) $3 (_ptInfo $2)) (reverse $5) $7 }

ImportList :: { Maybe [Text] }
  : '[' ImportNames ']' { Just (reverse $2) }
  | {- empty -} { Nothing }

ImportNames :: { [Text] }
  : ImportNames IDENT { (getIdent $2):$1 }
  | {- empty -} { [] }

DefConst :: { SpanInfo -> ParsedDefConst }
  : defconst IDENT MTypeAnn Expr MDoc { DefConst (MArg (getIdent $2) $3 (_ptInfo $2)) $4 $5 }

-- All defs
Defun :: { SpanInfo -> ParsedDefun }
  : defun IDENT MTypeAnn '(' MArgs ')' MDocOrModel Block
    { Defun (MArg (getIdent $2) $3 (_ptInfo $2)) (reverse $5) $8 $7 }

Defschema :: { SpanInfo -> DefSchema SpanInfo }
  : defschema IDENT MDocOrModel SchemaArgList
    { DefSchema (getIdent $2) (reverse $4) $3 }

Deftable :: { SpanInfo -> DefTable SpanInfo }
  : deftable IDENT ':' '{' ParsedName '}' MDoc { DefTable (getIdent $2) $5 $7 }

Defcap :: { SpanInfo -> DefCap SpanInfo }
  : defcap IDENT MTypeAnn '(' MArgs ')' MDocOrModel MDCapMeta Block
    { DefCap (MArg (getIdent $2) $3 (_ptInfo $2)) (reverse $5) $9 $7 $8 }

DefPact :: { SpanInfo -> DefPact SpanInfo }
  : defpact IDENT MTypeAnn '(' MArgs ')' MDocOrModel DefPactSteps
  { DefPact (MArg (getIdent $2) $3 (_ptInfo $2)) (reverse $5) $8 $7 }

DefPactSteps :: { NE.NonEmpty (PactStep SpanInfo) }
  : Steps { NE.fromList (reverse $1) }

Steps :: { [PactStep SpanInfo] }
  : Steps Step { $2:$1 }
  | Step { [$1] }

Step :: { PactStep SpanInfo }
  : '(' step Expr MModel ')' { Step $3 $4 }
  -- Note: this production which ignores its input
  -- is due to the legacy form of:
  -- (step ENTITY EXPR)
  | '(' step Expr Expr MModel ')' { Step $4 $5 }
  | '(' steprb Expr Expr MModel ')' { StepWithRollback $3 $4 $5 }
  -- (step-with-rollback ENTITY EXPR ROLLBACK-EXPR)
  -- hence we ignore entity
  | '(' steprb Expr Expr Expr MModel ')' { StepWithRollback $4 $5 $6 }

MDCapMeta :: { Maybe DCapMeta }
  : Managed { Just $1 }
  | Event { Just $1 }
  | {- empty -} { Nothing }

-- This production causes parser ambiguity.
-- Take the code:
-- (defcap f (a) @managed a a a)
-- is this an automanaged capability, or are the managed args capturing parameters.
Managed :: { DCapMeta }
  : managedAnn { DefManaged Nothing }
  | managedAnn IDENT ParsedName { DefManaged (Just (getIdent $2, $3)) }

Event :: { DCapMeta }
  : eventAnn { DefEvent }

MArgs :: { [MArg SpanInfo] }
  : MArgs MArg { $2:$1 }
  | {- empty -} { [] }

MArg :: { MArg SpanInfo }
  : IDENT ':' Type { MArg (getIdent $1) (Just $3) (_ptInfo $1)}
  | IDENT { MArg (getIdent $1) Nothing (_ptInfo $1)}

SchemaArgList :: { [Arg SpanInfo] }
  : SchemaArgList IDENT ':' Type { (Arg (getIdent $2) $4 (_ptInfo $2)):$1 }
  | SchemaArgList IDENT { (Arg (getIdent $2) TyAny (_ptInfo $2)):$1 }
  | {- empty -} { [] }

Type :: { Type }
  : '[' Type ']' { TyList $2 }
  | module '{' ModuleNames '}' { TyModRef (reverse $3) }
  | IDENT '{' ParsedTyName '}' {% objType (_ptInfo $1) (getIdent $1) $3}
  | IDENT {% primType (_ptInfo $1) (getIdent $1) }

ModuleNames :: { [ModuleName] }
  : ModuleNames ',' ModQual { (mkModName $3) : $1 }
  | ModQual { [mkModName $1] }

-- Annotations
DocAnn :: { Text }
  : docAnn STR { getStr $2 }

DocStr :: { Text }
  : STR { getStr $1 }

MModel :: { Maybe [PropertyExpr SpanInfo] }
  : ModelAnn { Just $1 }
  | {- empty -}  { Nothing }

ModelAnn :: { [PropertyExpr SpanInfo] }
  : modelAnn '[' PactFVModels ']' { $3 }

MDocOrModel :: { [PactAnn SpanInfo] }
  : DocAnn ModelAnn { [PactDoc PactDocAnn $1, PactModel $2] }
  | ModelAnn DocAnn { [PactModel $1, PactDoc PactDocAnn $2] }
  | DocAnn { [PactDoc PactDocAnn $1] }
  | ModelAnn { [PactModel $1] }
  | DocStr { [PactDoc PactDocString $1] }
  | {- empty -} { [] }

-- This production causes parser ambugity in two productions: defun and defcap
-- (defun f () "a")
-- (defcap f () "a")
-- it isn't clear whether "a" is a docstring or a string literal.
MDoc :: { Maybe (Text, PactDocType) }
  : DocAnn { Just ($1, PactDocAnn) }
  | DocStr { Just ($1, PactDocString) }
  | {- empty -} { Nothing }

MTypeAnn :: { Maybe Type }
  : ':' Type { Just $2 }
  | {- empty -} { Nothing }

Block :: { NE.NonEmpty ParsedExpr }
  : BlockBody { NE.fromList (reverse $1)  }

BlockBody :: { [ParsedExpr] }
  : BlockBody Expr { $2:$1 }
  | Expr { [$1] }

Expr :: { ParsedExpr }
  : '(' SExpr ')' { $2 (combineSpan (_ptInfo $1) (_ptInfo $3)) }
  | Atom { $1 }

SExpr :: { SpanInfo -> ParsedExpr }
  : LamExpr { $1 }
  | LetExpr { $1 }
  | GenAppExpr { $1 }

List :: { ParsedExpr }
  : '[' ListExprs ']' { List $2 (combineSpan (_ptInfo $1) (_ptInfo $3)) }

ListExprs :: { [ParsedExpr] }
  : Expr MCommaExpr { $1:(reverse $2) }
  | {- empty -} { [] }

MCommaExpr :: { [ParsedExpr] }
  : ',' ExprCommaSep { $2 }
  | AppList { $1 }

ExprCommaSep :: { [ParsedExpr] }
  : ExprCommaSep ',' Expr { $3:$1 }
  | Expr { [$1] }

LamExpr :: { SpanInfo -> ParsedExpr }
  : lam '(' LamArgs ')' Block { Lam (reverse $3) $5 }

LamArgs :: { [MArg SpanInfo] }
  : LamArgs IDENT ':' Type { (MArg (getIdent $2) (Just $4) (_ptInfo $2)):$1 }
  | LamArgs IDENT { (MArg (getIdent $2) Nothing (_ptInfo $2)):$1 }
  | {- empty -} { [] }

LetExpr :: { SpanInfo -> ParsedExpr }
  : let '(' Binders ')' Block { Let LFLetNormal (NE.fromList (reverse $3)) $5 }
  | letstar '(' Binders ')' Block { Let LFLetStar (NE.fromList (reverse $3)) $5 }

Binders :: { [Binder SpanInfo] }
  : Binders '(' IDENT MTypeAnn Expr ')' { (Binder (getIdent $3) $4 $5):$1 }
  | '(' IDENT MTypeAnn Expr ')' { [Binder (getIdent $2) $3 $4] }

GenAppExpr :: { SpanInfo -> ParsedExpr }
  : Expr AppBindList { \i -> App $1 (toAppExprList i (reverse $2)) i }

AppList :: { [ParsedExpr] }
  : AppList Expr { $2:$1 }
  | {- empty -} { [] }

AppBindList :: { [Either ParsedExpr [(Field, MArg SpanInfo)]] }
  : AppBindList Expr { (Left $2):$1 }
  | AppBindList BindingForm { (Right $2):$1}
  | {- empty -} { [] }

BindingForm :: { [(Field, MArg SpanInfo)] }
  : '{' BindPairs '}' { $2 }

BindPair :: { (Field, MArg SpanInfo) }
  : STR ':=' MArg { (Field (getStr $1), $3) }
  | TICK ':=' MArg { (Field (getTick $1), $3) }

BindPairs :: { [(Field, MArg SpanInfo)] }
  : BindPairs ',' BindPair { $3 : $1 }
  | BindPair { [$1] }


Atom :: { ParsedExpr }
  : Var { $1 }
  | Number { $1 }
  | String { $1 }
  | List { $1 }
  | Bool { $1 }
  | Object { $1 }
  | '(' ')' { Constant LUnit (_ptInfo $1) }


Bool :: { ParsedExpr }
  : true { Constant (LBool True) (_ptInfo $1) }
  | false { Constant (LBool False) (_ptInfo $1) }


Var :: { ParsedExpr }
  : IDENT '.' ModQual  { Var (QN (mkQualName (getIdent $1) $3)) (combineSpan (_ptInfo $1) (view _3 $3))  }
  | IDENT { Var (BN (mkBarename (getIdent $1))) (_ptInfo $1) }
  | IDENT '::' IDENT { Var (DN (DynamicName (getIdent $1) (getIdent $3))) (combineSpan (_ptInfo $1) (_ptInfo $3)) }

ParsedName :: { ParsedName }
  : IDENT '.' ModQual { QN (mkQualName (getIdent $1) $3) }
  | IDENT { BN (mkBarename (getIdent $1)) }
  | IDENT '::' IDENT { DN (DynamicName (getIdent $1) (getIdent $3)) }

ParsedTyName :: { ParsedTyName }
  : IDENT '.' ModQual { TQN (mkQualName (getIdent $1) $3) }
  | IDENT { TBN (mkBarename (getIdent $1)) }

ModQual :: { (Text, Maybe Text, SpanInfo) }
  : IDENT '.' IDENT { (getIdent $1, Just (getIdent $3), (combineSpan (_ptInfo $1) (_ptInfo $3))) }
  | IDENT { (getIdent $1, Nothing, _ptInfo $1) }

Number :: { ParsedExpr }
  : NUM '.' NUM {% mkDecimal Constant (getNumber $1) (getNumber $3) (_ptInfo $1) }
  | NUM { mkIntegerConstant Constant (getNumber $1) (_ptInfo $1) }

String :: { ParsedExpr }
 : STR  { Constant (LString (getStr $1)) (_ptInfo $1) }
 | TICK { Constant (LString (getTick $1)) (_ptInfo $1) }

Object :: { ParsedExpr }
  : '{' ObjectBody '}' { Object $2 (combineSpan (_ptInfo $1) (_ptInfo $3)) }

ObjectBody :: { [(Field, ParsedExpr)] }
  : FieldPairs { $1 }

FieldPair :: { (Field, ParsedExpr) }
  : STR ':' Expr { (Field (getStr $1), $3) }
  | TICK ':' Expr { (Field (getTick $1), $3) }

FieldPairs :: { [(Field, ParsedExpr)] }
  : FieldPairs ',' FieldPair { $3 : $1 }
  | FieldPair { [$1] }
  | {- empty -} { [] }

PactFVModels :: { [PropertyExpr SpanInfo] }
  : PropExprList { reverse $1 }

PropExprList :: { [PropertyExpr SpanInfo] }
  : PropExprList PropExpr { $2:$1 }
  | {- empty -} { [] }

PropExpr :: { PropertyExpr SpanInfo }
  : PropAtom { $1 }
  | '(' PropExprList ')' { PropSequence (reverse $2) (combineSpan (_ptInfo $1) (_ptInfo $3)) }
  | '[' PropExprList ']' { propExprList $1 (reverse $2) $3 }


PropAtom :: { PropertyExpr SpanInfo }
  : FVVar { uncurry PropAtom $1  }
  | FVNumber { $1 }
  | FVString { $1 }
  | FVKeyword { $1 }
  | FVDelim { $1 }
  | FVBool { $1 }

FVKeyword :: { PropertyExpr SpanInfo }
  : let { PropKeyword KwLet (_ptInfo $1) }
  | lam { PropKeyword KwLambda (_ptInfo $1) }

FVDelim :: { PropertyExpr SpanInfo }
  : '{' { PropDelim DelimLBrace (_ptInfo $1) }
  | '}' { PropDelim DelimRBrace (_ptInfo $1) }
  | ':' { PropDelim DelimColon (_ptInfo $1) }
  | ',' { PropDelim DelimComma (_ptInfo $1) }

FVBool :: { PropertyExpr SpanInfo }
  : true { PropConstant (LBool True) (_ptInfo $1) }
  | false { PropConstant (LBool False) (_ptInfo $1) }

FVNumber :: { PropertyExpr SpanInfo }
  : NUM '.' NUM {% mkDecimal PropConstant (getNumber $1) (getNumber $3) (_ptInfo $1) }
  | NUM { mkIntegerConstant PropConstant (getNumber $1) (_ptInfo $1) }

FVString :: { PropertyExpr SpanInfo }
 : STR  { PropConstant (LString (getStr $1)) (_ptInfo $1) }
 | TICK { PropConstant (LString (getTick $1)) (_ptInfo $1) }


FVVar :: { (ParsedName, SpanInfo) }
  -- Todo: modqual spaninfos fix here
  -- fixed in https://github.com/kadena-io/pact-core/pull/63
  : IDENT '.' ModQual  { (QN (mkQualName (getIdent $1) $3), _ptInfo $1) }
  | IDENT { (BN (mkBarename (getIdent $1)), _ptInfo $1) }
  | IDENT '::' IDENT { (DN (DynamicName (getIdent $1) (getIdent $3)), combineSpan (_ptInfo $1) (_ptInfo $3)) }

{

combineSpans lexpr rexpr =
  let li = view termInfo lexpr
      ri = view termInfo rexpr
  in combineSpan li ri

getIdent (PosToken (TokenIdent x) _) = x
getNumber (PosToken (TokenNumber x) _) = x
getStr (PosToken (TokenString x) _ ) = x
getTick (PosToken (TokenSingleTick x) _) = T.drop 1 x
getIdentField = Field . getIdent

mkIntegerConstant ctor n i =
  let (n', f) = if T.head n == '-' then (T.drop 1 n, negate) else (n, id)
      strToNum = T.foldl' (\x d -> 10*x + toInteger (digitToInt d))
  in ctor (LInteger (f (strToNum 0 n'))) i

mkDecimal ctor num dec i = do
  let (num', f) = if T.head num == '-' then (T.drop 1 num, negate) else (num, id)
      strToNum = T.foldl' (\x d -> 10*x + toInteger (digitToInt d))
      prec = T.length dec
  when (prec > 255) $ throwParseError (PrecisionOverflowError prec) i
  let out = Decimal (fromIntegral prec) (f (strToNum (strToNum 0 num') dec))
  pure $ ctor (LDecimal out) i

mkQualName ns (mod, (Just ident), _) =
  let ns' = NamespaceName ns
  in QualifiedName ident (ModuleName mod (Just ns'))
mkQualName mod (ident, Nothing, _) =
  QualifiedName ident (ModuleName mod Nothing)

mkQualName' ns (mod, (Just ident)) =
  let ns' = NamespaceName ns
  in QualifiedName ident (ModuleName mod (Just ns'))
mkQualName' mod (ident, Nothing) = QualifiedName ident (ModuleName mod Nothing)


mkModName (ident, Nothing, _) = ModuleName ident Nothing
mkModName (ns, Just ident, _) = ModuleName ident (Just (NamespaceName ns))

propExprList tokLBracket li tokRBracket =
  let lbracket = PropDelim DelimLBracket (_ptInfo tokLBracket)
      rbracket = PropDelim DelimRBracket (_ptInfo tokRBracket)
      finfo = combineSpan (_ptInfo tokLBracket) (_ptInfo tokRBracket)
  in PropSequence ((lbracket:li)++[rbracket]) finfo

mkBarename tx = BareName tx


}
