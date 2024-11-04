{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Pact.Core.Errors
 ( PactErrorI
 , LexerError(..)
 , ParseError(..)
 , DesugarError(..)
 , InvariantError(..)
 , EvalError(..)
 , PactError(..)
 , ArgTypeError(..)
 , DbOpError(..)
 , HyperlaneError(..)
 , HyperlaneDecodeError(..)
 , OwneraError(..)
 , peInfo
 , viewErrorStack
 , UserRecoverableError(..)
 , ErrorCode(..)
 , PactErrorCode(..)
 , PrettyErrorCode(..)
 , pactErrorToErrorCode
 , prettyErrorCode
 , errorCodeFromText
 , LegacyPactError(..)
 , LegacyPactErrorType(..)
 , PactErrorCompat(..)
 , VerifierError(..)
 , _PELexerError
 , _PEParseError
 , _PEDesugarError
 , _PEExecutionError
 , _PEUserRecoverableError
 , _InvariantInvalidDefKind
 , _InvariantDefConstNotEvaluated
 , _InvariantExpectedDefCap
 , _InvariantExpectedDefun
 , _InvariantExpectedDefPact
 , _InvariantInvalidBoundVariable
 , _InvariantUnboundFreeVariable
 , _InvariantMalformedDefun
 , _InvariantPactExecNotInEnv
 , _InvariantPactStepNotInEnv
 , _InvariantInvalidManagedCapIndex
 , _InvariantArgLengthMismatch
 , _InvariantInvalidManagedCapKind
 , _InvariantNoSuchKeyInTable
 , _InvariantEmptyCapStackFailure
 , _ParsingError
 , _TooManyCloseParens
 , _UnexpectedInput
 , _PrecisionOverflowError
 , _InvalidBaseType
 , _LexicalError
 , _StringLiteralError
 , _OutOfInputError
 , _UnboundTermVariable
 , _UnboundTypeVariable
 , _InvalidCapabilityReference
 , _NoSuchModuleMember
 , _NoSuchModule
 , _NoSuchInterface
 , _ImplementationError
 , _NotImplemented
 , _RecursionDetected
 , _NotAllowedWithinDefcap
 , _NotAllowedOutsideModule
 , _InvalidGovernanceRef
 , _InvalidDefInTermVariable
 , _InvalidModuleReference
 , _EmptyBindingBody
 , _LastStepWithRollback
 , _ExpectedFreeVariable
 , _InvalidManagedArg
 , _InvalidImports
 , _InvalidImportModuleHash
 , _InvalidSyntax
 , _InvalidDefInSchemaPosition
 , _InvalidDynamicInvoke
 , _DuplicateDefinition
 , _InvalidBlessedHash
 , _ArrayOutOfBoundsException
 , _ArithmeticException
 , _EnumerationError
 , _DecodeError
 , _GasExceeded
 , _FloatingPointError
 , _InvariantFailure
 , _EvalError
 , _NativeArgumentsError
 , _InvalidManagedCap
 , _CapNotInstalled
 , _CapAlreadyInstalled
 , _ModuleMemberDoesNotExist
 , _NoSuchKeySet
 , _YieldOutsideDefPact
 , _NoActiveDefPactExec
 , _NoYieldInDefPactStep
 , _InvalidDefPactStepSupplied
 , _DefPactIdMismatch
 , _CCDefPactContinuationError
 , _NoPreviousDefPactExecutionFound
 , _DefPactAlreadyCompleted
 , _NestedDefPactParentStepCountMismatch
 , _NestedDefPactParentRollbackMismatch
 , _NestedDefPactNeverStarted
 , _NestedDefPactDoubleExecution
 , _MultipleOrNestedDefPactExecFound
 , _DefPactStepHasNoRollback
 , _DefPactStepNotInEnvironment
 , _NoDefPactIdAndExecEnvSupplied
 , _DefPactRollbackMismatch
 , _DefPactStepMismatch
 , _CannotUpgradeInterface
 , _DbOpFailure
 , _DynNameIsNotModRef
 , _ModuleDoesNotExist
 , _ExpectedModule
 , _HashNotBlessed
 , _CannotApplyPartialClosure
 , _ClosureAppliedToTooManyArgs
 , _FormIllegalWithinDefcap
 , _RunTimeTypecheckFailure
 , _NativeIsTopLevelOnly
 , _EventDoesNotMatchModule
 , _InvalidEventCap
 , _NestedDefpactsNotAdvanced
 , _ExpectedPactValue
 , _NotInDefPactExecution
 , _RootNamespaceInstallError
 , _PointNotOnCurve
 , _YieldProvenanceDoesNotMatch
 , _MismatchingKeysetNamespace
 , _EnforcePactVersionFailure
 , _EnforcePactVersionParseFailure
 , _RuntimeRecursionDetected
 , _SPVVerificationFailure
 , _ContinuationError
 , _UserGuardMustBeADefun
 , _ExpectedBoolValue
 , _ExpectedStringValue
 , _ExpectedCapToken
 , _WriteValueDidNotMatchSchema
 , _ObjectIsMissingField
 , _InvalidKeysetFormat
 , _InvalidKeysetNameFormat
 , _CannotDefineKeysetOutsideNamespace
 , _NamespaceNotFound
 , _NativeExecutionError
 , _OperationIsLocalOnly
 , _CannotApplyValueToNonClosure
 , _InvalidCustomKeysetPredicate
 , _HyperlaneError
 , _HyperlaneDecodeError
 , _OwneraError
 , _UnknownException
 , _UserEnforceError
 , _OneShotCapAlreadyUsed
 , _CapabilityNotGranted
 , _NoSuchObjectInDb
 , _KeysetPredicateFailure
 , _CapabilityPactGuardInvalidPactId
 , _EnvReadFunctionFailure
 , _VerifierFailure
 , _CapabilityGuardNotAcquired
 , _HyperlaneErrorFailedToFindKey
 , _HyperlaneErrorNumberOutOfBounds
 , _HyperlaneErrorBadHexPrefix
 , _HyperlaneErrorInvalidBase64
 , _HyperlaneErrorIncorrectSize
 , _HyperlaneErrorInvalidChainId
 , _HyperlaneDecodeErrorBase64
 , _HyperlaneDecodeErrorInternal
 , _HyperlaneDecodeErrorBinary
 , _HyperlaneDecodeErrorParseRecipient
 , toPrettyLegacyError
 , BoundedText
 , _boundedText
 , mkBoundedText
 , PactErrorOrigin(..)
 , LocatedErrorInfo(..)
 , pactErrorToLocatedErrorCode
 ) where

import Control.Lens hiding (ix)
import Control.Monad
import Control.Applicative
import Data.Bits
import Data.Foldable(find)
import Data.Proxy
import Data.Text(Text)
import Data.Typeable(Typeable)
import Data.Set(Set)
import Data.Word
import Data.List(intersperse)
import Data.Default
import Numeric (showHex)
import GHC.TypeLits
import qualified Data.Version as V
import qualified Pact.Core.Version as PI
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Char as C
import qualified Pact.JSON.Decode as JD
import qualified Pact.JSON.Encode as J

import Control.DeepSeq
import GHC.Generics

import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Info
import Pact.Core.Pretty as Pretty
import Pact.Core.Hash
import Pact.Core.StackFrame
import Pact.Core.DefPacts.Types
import Pact.Core.PactValue
import Pact.Core.Capabilities
import Pact.Core.DeriveConTag
import Pact.Core.ChainData (ChainId(_chainId))
import Data.String (IsString(..))
import Pact.Core.Gas.Types

-- A common type alias, as `PactErrorI`s come straight from running
-- the lexer + parser
type PactErrorI = PactError SpanInfo

-- | Errors that occur during lexing
data LexerError
  = LexicalError Char Char
  -- ^ Lexical error: encountered character, last seen character
  | StringLiteralError Text
  -- ^ Error lexing string literal
  | OutOfInputError Char
  deriving (Eq, Show, Generic)

instance NFData LexerError


instance Pretty LexerError where
  pretty = ("Lexical Error: " <>) . \case
    LexicalError c1 c2 ->
      Pretty.hsep ["Encountered unexpected character",  Pretty.dquotes (pretty c1) <> ",", "Last seen", Pretty.dquotes (pretty c2)]
    StringLiteralError te ->
      Pretty.hsep ["String literal parsing error: ", pretty te]
    OutOfInputError c ->
      Pretty.hsep ["Ran out of input before finding a lexeme. Last Character seen: ", Pretty.parens (pretty c)]

-- | Errors that occur during parsing
data ParseError
  = ParsingError Text
  -- ^ Parsing error: [expected]
  | TooManyCloseParens Text
  -- ^ Too many closing parens [Remaining t]
  | UnexpectedInput Text
  -- Todo: Potentially the error here would be better if
  -- ^ Too much input in general. Did not expect more tokens.
  -- Emitted in the case of "Expression was parsed successfully but there's more input remaining."
  | PrecisionOverflowError Int
  -- ^ Way too many decimal places for `Decimal` to deal with, max 255 precision.
  | InvalidBaseType Text
  -- ^ Invalid primitive type
  deriving (Eq, Show, Generic)

instance NFData ParseError


instance Pretty ParseError where
  pretty = \case
    ParsingError e ->
      Pretty.hsep ["Expected:", pretty e]
    TooManyCloseParens e ->
      Pretty.hsep ["Too many closing parens, remaining tokens:", pretty e]
    UnexpectedInput e ->
      Pretty.hsep ["Unexpected input after expr, remaining tokens:", pretty e]
    PrecisionOverflowError i ->
      Pretty.hsep ["Precision overflow (>=255 decimal places): ", pretty i, "decimals"]
    InvalidBaseType txt ->
      Pretty.hsep ["No such type:", pretty txt]

-- | Desugar errors are raised from our name resolution + desugaring pass.
--   they mostly encompass things like unbound variables, modules that don't exist,
--   and invalid use of special syntactic forms (Extensions to lisp that are pact-specific)
data DesugarError
  = UnboundTermVariable Text
  -- ^ Encountered a variable with no binding <varname>
  | UnboundTypeVariable Text
  -- ^ Found an unbound type variable in a type definition
  -- (note: in this current version of core this should never occur,
  --  there are no userland type variables that can be annotated)
  | InvalidCapabilityReference Text
  -- ^ Function <function name> is used in a scope that expected a capability
  | NoSuchModuleMember ModuleName Text
  -- ^ Module <modulename> does not have member <membername>
  | NoSuchModule ModuleName
  -- ^ Module <modulename> does not exist
  | NoSuchInterface ModuleName
  -- ^ Interface <ifname> doesnt exist
  | ImplementationError ModuleName ModuleName Text
  -- ^ Interface implemented in module for member <member> does not match the signature
  | NotImplemented ModuleName ModuleName Text
  -- ^ Interface member not implemented
  | RecursionDetected ModuleName [Text]
  -- ^ Detected use of recursion in module <module>. [functions] for a cycle
  | NotAllowedWithinDefcap Text
  -- ^ Form <text> not allowed within defcap
  | NotAllowedOutsideModule Text
  -- ^ Form not allowed outside of module call <description
  | InvalidGovernanceRef QualifiedName
  -- ^ No such governance
  | InvalidDefInTermVariable Text
  -- ^ Invalid top level defn references a non-semantic value (e.g defcap, defschema)
  | InvalidModuleReference ModuleName
  -- ^ Invalid: Interface used as module reference
  | EmptyBindingBody
  -- ^ Binding form has no expressions to bind to
  | LastStepWithRollback QualifiedName
  -- ^ Last Step has Rollback error
  | ExpectedFreeVariable Text
  -- ^ Expected free variable for ident
  | InvalidManagedArg Text
  -- ^ Invalid @managed argument, there is no named argument with name <name>
  | InvalidImports ModuleName [Text]
  -- ^ Imports for <module> do not exist <imports>
  | InvalidImportModuleHash ModuleName ModuleHash
  -- ^ Expected free variable
  | InvalidSyntax Text
  -- ^ Desugaring failed on invalid syntactic transformation (e.g within `cond`)
  | InvalidDefInSchemaPosition Text
  -- ^ Found a non-defschema in a type name position
  | InvalidDynamicInvoke DynamicName
  -- ^ Dynamic invoke is invalid
  | DuplicateDefinition QualifiedName
  -- ^ Name was defined twice
  | InvalidBlessedHash Text
  -- ^ Blessed hash has invalid format
  deriving (Eq, Show,  Generic)

instance NFData DesugarError


instance Pretty DesugarError where
  pretty = \case
    UnboundTermVariable t ->
      Pretty.hsep ["Unbound variable", pretty t]
    UnboundTypeVariable t ->
      Pretty.hsep ["Unbound type variable", pretty t]
    InvalidCapabilityReference t ->
      Pretty.hsep ["Variable or function used in special form is not a capability", pretty t]
    NoSuchModuleMember mn txt ->
      Pretty.hsep ["Module", pretty mn, "has no such member:", pretty txt]
    NoSuchModule mn ->
      Pretty.hsep ["Cannot find module: ", pretty mn]
    NoSuchInterface mn ->
      Pretty.hsep ["Cannot find interface: ", pretty mn]
    NotAllowedWithinDefcap dc ->
      Pretty.hsep [pretty dc, "form not allowed within defcap"]
    NotAllowedOutsideModule txt ->
      Pretty.hsep [pretty txt, "not allowed outside of a module"]
    ImplementationError mn1 mn2 defn ->
      Pretty.hsep [ "Module"
                   , pretty mn1
                   , "does not correctly implement the function"
                   , pretty defn
                   , "from Interface"
                   , pretty mn2]
    RecursionDetected mn txts ->
      Pretty.hsep
      ["Recursive cycle detected in Module"
      , pretty mn
      , "in the following functions:"
      , pretty txts]
    InvalidGovernanceRef gov ->
      Pretty.hsep ["Invalid governance. Must be a 0-argument defcap or a keyset. Found", pretty gov]
    InvalidDefInTermVariable n ->
      Pretty.hsep ["Invalid definition in term variable position:", pretty n]
    InvalidModuleReference mn ->
      Pretty.hsep ["Invalid Interface attempted to be used as module reference:", pretty mn]
    EmptyBindingBody -> "Bind expression lacks an accompanying body"
    LastStepWithRollback mn ->
      Pretty.hsep ["rollbacks aren't allowed on the last step in:", pretty mn]
    ExpectedFreeVariable t ->
      Pretty.hsep ["Expected free variable in expression, found locally bound: ", pretty t]
    -- Todo: pretty these
    InvalidManagedArg arg ->
      "Invalid Managed arg: no such arg with name" <+> pretty arg
    NotImplemented mn ifn ifdn ->
      "Interface member not implemented, module" <+> pretty mn <+> "does not implement interface"
      <+> pretty ifn <+> "member:" <+> pretty ifdn
    InvalidImports mn imps ->
      "Invalid imports, module or interface" <+> pretty mn <+> "does not implement the following members:"
      <+> pretty imps
    InvalidImportModuleHash mn mh ->
      "Import error for module" <+> pretty mn <+> ", hash not blessed:"
        <+> pretty mh
    InvalidSyntax msg ->
      "Desugar syntax failure:" <+> pretty msg
    InvalidDefInSchemaPosition n ->
      "Invalid def in defschema position:" <+> pretty n <+> "is not a valid schema"
    InvalidDynamicInvoke dn ->
      "Invalid dynamic call:" <+> pretty dn
    DuplicateDefinition qn ->
      "Duplicate definition:" <+> pretty qn
    InvalidBlessedHash hs ->
      "Invalid blessed hash, incorrect format:" <+> pretty hs

-- | Argument type mismatch meant for errors
--   that does not force you to show the whole PactValue
data ArgTypeError
  = ATEPrim PrimType
  | ATEList
  | ATEObject
  | ATETable
  | ATEClosure
  | ATEModRef
  | ATECapToken
  deriving (Eq, Show,  Generic)

instance NFData ArgTypeError

pactValueToArgTypeError :: PactValue -> ArgTypeError
pactValueToArgTypeError = \case
  PLiteral l -> ATEPrim (literalPrim l)
  PTime _ -> ATEPrim PrimTime
  PList _ -> ATEList
  PObject _ -> ATEObject
  PGuard _ -> ATEPrim PrimGuard
  PModRef _ -> ATEModRef
  PCapToken _ -> ATEClosure

typeToArgTypeError :: Type -> ArgTypeError
typeToArgTypeError = \case
  TyPrim p -> ATEPrim p
  TyList _ -> ATEList
  TyObject _ -> ATEObject
  TyAnyObject -> ATEObject
  TyAnyList -> ATEList
  TyCapToken -> ATECapToken
  -- This case should never happen
  TyAny -> ATEObject
  TyModRef _ -> ATEModRef
  TyTable _ -> ATETable


renderArgTypeError :: ArgTypeError -> Text
renderArgTypeError = \case
  ATEPrim p -> renderPrimType p
  ATEList -> "list"
  ATEObject -> "object"
  ATETable -> "table"
  ATEClosure -> "closure"
  ATEModRef -> "modref"
  ATECapToken -> "cap-token"

instance Pretty ArgTypeError where
  pretty = \case
    ATEPrim p -> pretty p
    ATEList -> "list"
    ATEObject -> "object"
    ATETable -> "table"
    ATEClosure -> "closure"
    ATEModRef -> "modref"
    ATECapToken -> "cap-token"

-- | Invariant errors are _not_ meant to happen, ever, during pact execution.
--   They handle cases like expecting a value on the stack that is not there,
--   a malformed definition and other such invariants.
--
--   Any time an `InvariantError` occurs after a particular piece of source code
--   has gone through our interpretation pipeline, it is a bug and should be raised as an issue.
data InvariantError
  = InvariantInvalidDefKind DefKind Text
  -- ^ An illegal `def` form was found in a term variable position
  -- | InvariantNameNotInScope FullyQualifiedName
  -- ^ Unbound free
  | InvariantDefConstNotEvaluated FullyQualifiedName
  -- ^ Defconst found but was not evaluated
  -- note this is an invariant failure because evaluating a defconst term during
  -- regular runtime execution has different semantics to pre-evaluating it.
  | InvariantExpectedDefCap FullyQualifiedName
  -- ^ Expected a defcap in some position (e.g evalCAp)
  | InvariantExpectedDefun FullyQualifiedName
  -- ^ Expected a defun in some position (e.g user managed cap)
  | InvariantExpectedDefPact FullyQualifiedName
  -- ^ Expected a defcap in some position (e.g applyPact)
  | InvariantInvalidBoundVariable Text
  -- ^ Bound variable has no accompanying binder
  | InvariantUnboundFreeVariable FullyQualifiedName
  -- ^ Unbound free variable
  | InvariantMalformedDefun FullyQualifiedName
  -- ^ Defun term is malformed somehow (e.g no bound variables at all, not a nullary closure)
  | InvariantPactExecNotInEnv (Maybe (DefPactContinuation QualifiedName PactValue))
  -- ^ Defpact Exec expected to be in environment but not found
  | InvariantPactStepNotInEnv (Maybe (DefPactContinuation QualifiedName PactValue))
  -- ^ Defpact Exec expected to be in environment but not found
  | InvariantInvalidManagedCapIndex Int FullyQualifiedName
  -- ^ managed cap index outside of allowable range
  | InvariantArgLengthMismatch FullyQualifiedName Int Int
  -- ^ Argument length mismatch within some internal function
  | InvariantInvalidManagedCapKind Text
  -- ^ Invariant managed cap kind, expected, got <msg>
  | InvariantNoSuchKeyInTable TableName RowKey
  -- ^ Keys were pulled from the table (e.g select, fold-db) but
  -- there was no corresponding entry
  | InvariantEmptyCapStackFailure
  -- ^ Attempted to pop or manipulate the capstack, but it was found to be empty
  deriving (Eq, Show, Generic)

instance NFData InvariantError

instance Pretty InvariantError where
  pretty = \case
    InvariantInvalidDefKind dk t ->
      "Invalid def kind, received" <+> pretty dk <+> pretty t
    InvariantDefConstNotEvaluated fqn ->
      "Defconst was not evaluated prior to execution:" <+> pretty fqn
    InvariantExpectedDefCap fqn ->
      "Expected a defcap for free variable" <+> pretty fqn
    InvariantExpectedDefun fqn ->
      "Expected a defun for free variable" <+> pretty fqn
    InvariantExpectedDefPact fqn ->
      "Expected a defpact for free variable" <+> pretty fqn
    InvariantUnboundFreeVariable fqn ->
      "Unbound free variable" <+> pretty fqn
    InvariantInvalidBoundVariable v ->
      "Invalid bound or free variable:" <+> pretty v
    InvariantMalformedDefun dfn ->
      "Malformed defun: Body is not a lambda" <+> pretty dfn
    InvariantPactExecNotInEnv loc ->
      "No pact exec found" <+> pretty loc
    InvariantPactStepNotInEnv loc ->
      "No pact stepfound" <+> pretty loc
    InvariantInvalidManagedCapIndex i fqn ->
      "Invalid managed cap argument index" <+> pretty i <+> "for function" <+> pretty fqn
    InvariantArgLengthMismatch fqn expected got ->
      "Argument length mismatch for" <+> pretty fqn <> ", expected" <+> pretty expected <> ", got"
        <+> pretty got
    InvariantInvalidManagedCapKind msg ->
      "Invalid managed cap kind" <+> pretty msg
    InvariantNoSuchKeyInTable tbl (RowKey rk) ->
      "No such key" <+> pretty rk <+> "in table" <+> pretty tbl
    InvariantEmptyCapStackFailure ->
      "Attempted to pop or manipulate the capstack, but it was found to be empty"


-- | All fatal execution errors that can be raised during pact execution
data EvalError
  = ArrayOutOfBoundsException Int Int
  -- ^ Array index out of bounds <length> <index>
  | ArithmeticException Text
  -- ^ Arithmetic error <cause>
  | EnumerationError Text
  -- ^ Enumeration error (e.g incorrect bounds with step
  | DecodeError Text
  -- ^ Some form of decoding error
  | GasExceeded GasLimit Gas
  -- ^ Gas went past the gas limit
  | FloatingPointError Text
  -- ^ Floating point operation exception
  | InvariantFailure InvariantError
  -- ^ Invariant violation in execution. This is a fatal Error.
  | EvalError Text
  -- ^ Error raised by the program that went unhandled
  | NativeArgumentsError NativeName [ArgTypeError]
  -- ^ Error raised: native called with the wrong arguments
  -- ^ Module reference not refined to a value
  | InvalidManagedCap FullyQualifiedName
  -- ^ Name does not point to a managed capability
  | CapNotInstalled (CapToken QualifiedName PactValue)
  -- ^ Capability not installed
  | CapAlreadyInstalled (CapToken QualifiedName PactValue)
  -- ^ Capability already installed
  | ModuleMemberDoesNotExist FullyQualifiedName
  -- ^ Name not found in the top level environment
  | NoSuchKeySet KeySetName
    -- ^ No such keyset
  | YieldOutsideDefPact
  -- ^ Yield a value outside a running DefPactExec
  | NoActiveDefPactExec
  -- ^ No Active DefPactExec in the environment
  | NoYieldInDefPactStep DefPactStep
  -- ^ No Yield available in DefPactStep
  | InvalidDefPactStepSupplied DefPactStep Int
  -- ^ Supplied DefPactStep requests an invalid step, stepCount
  | DefPactIdMismatch DefPactId DefPactId
  -- ^ Requested PactId does not match context PactId
  | CCDefPactContinuationError DefPactStep DefPactExec DefPactExec
  -- ^ Crosschain defpact contunation must be at least 2 steps before CC continuation step
  --   with <ccExec> <dbExec>
  | NoPreviousDefPactExecutionFound DefPactStep
  -- ^ No previouse defpact execution could be found in the environment or database
  | DefPactAlreadyCompleted DefPactStep
  -- ^ defpact already completed
  | NestedDefPactParentStepCountMismatch DefPactId Int Int
  -- ^ Nested defpact <stepcount> does not match <parent step count>
  | NestedDefPactParentRollbackMismatch DefPactId Bool Bool
  -- ^ Nested defpact <rollback> does not match <parent rollback>
  | NestedDefPactNeverStarted DefPactStep
  -- ^ Nested defpact never started at prior step
  | NestedDefPactDoubleExecution DefPactStep
  -- ^ Nested defpact is executed twice
  | MultipleOrNestedDefPactExecFound DefPactExec
  -- ^ Unexpected DefPactExec found in the environment
  | DefPactStepHasNoRollback DefPactStep
  -- ^ The requested DefPactStep has no rollback
  | DefPactStepNotInEnvironment
  -- ^ DefPactStep is not in the environment
  | NoDefPactIdAndExecEnvSupplied
  -- ^ No DefPactId supplied and no DefPactExec found in the environment
  | DefPactRollbackMismatch DefPactStep DefPactExec
  -- ^ defpact rollback mismatch
  | DefPactStepMismatch DefPactStep DefPactExec
  -- ^ defpact mismatch
  | CannotUpgradeInterface ModuleName
  -- ^ Interface cannot be upgrade
  | DbOpFailure DbOpError
  -- ^ Db operation failure
  | DynNameIsNotModRef Text
  -- ^ Dynamic name does not point to a module reference
  | ModuleDoesNotExist ModuleName
  -- ^ Module was not found in the db nor in the environment
  | ExpectedModule ModuleName
  -- ^ Expected Module, found interface
  | HashNotBlessed ModuleName ModuleHash
  -- ^ Hash not blessed for <module> at <hash>
  | CannotApplyPartialClosure
  -- ^ Intentional nerf to partially applied closures
  -- outside of native code
  | ClosureAppliedToTooManyArgs
  -- ^ Closure called with too many arguments
  | FormIllegalWithinDefcap Text
  -- ^ Invalid function within a defcap
  | RunTimeTypecheckFailure ArgTypeError Type
  -- ^ Runtime TC failure, note: ArgTypeError simply allows us to
  --   abbreviate the type of the argument, instead of fully syntesizing it
  | NativeIsTopLevelOnly NativeName
  -- ^ Native called within module scope
  | EventDoesNotMatchModule ModuleName
  -- ^ Emitted event does not match the emitting module, or
  -- is called outside of module
  | InvalidEventCap FullyQualifiedName
  -- ^ Capability is not @event or @managed, thus it should not be able
  --   to emit an event
  | NestedDefpactsNotAdvanced DefPactId
  -- ^ Nested defpact not advanced. Note: All nested defpacts
  -- semantically must be advanced in lockstep. That is, for some defpact execution at
  -- step n, all nested defpacts prior to execution at step (n-1) should also be advanced to
  -- step n at the end of execution
  | ExpectedPactValue
  -- ^ Expected a pact value, received a closure or table reference
  | NotInDefPactExecution
  -- ^  Expected function to be called within a defpact. E.g (pact-id)
  | RootNamespaceInstallError
  -- ^ Error installing namespace
  | PointNotOnCurve
  -- ^ Pairing-related: Point lies outside of elliptic curve
  | YieldProvenanceDoesNotMatch Provenance [Provenance]
  -- ^ Yield provenance mismatch
  | MismatchingKeysetNamespace NamespaceName
  -- ^ Keyset declared outside of relevant namespace
  | EnforcePactVersionFailure V.Version (Maybe V.Version)
  -- ^ Pact version fails
  | EnforcePactVersionParseFailure Text
  -- ^ Pact version parsing error
  | RuntimeRecursionDetected QualifiedName
  -- ^ Attempted to call <function> recursively
  | SPVVerificationFailure Text
  -- ^ Failure in SPV verification
  | ContinuationError Text
  -- ^ Failure in evalContinuation (chainweb)
  | UserGuardMustBeADefun QualifiedName DefKind
  -- ^ User guard closure must refer to a defun
  | ExpectedBoolValue PactValue
  -- ^ Expected a boolean result in evaluation
  -- (e.g if, or, and)
  | ExpectedStringValue PactValue
  -- ^ Expected a string value during evaluation
  -- (e.g enforce)
  | ExpectedCapToken PactValue
  -- ^ Expected a string value during evaluation
  -- (e.g enforce)
  | WriteValueDidNotMatchSchema Schema (ObjectData PactValue)
  -- ^ Attempted to write a value to the database that does not match
  -- the database's schema
  | ObjectIsMissingField Field (ObjectData PactValue)
  -- ^ Object access is missing a field
  | InvalidKeysetFormat KeySet
  -- ^ Keyset format validation failure (e.g ED25519Hex or Webauthn)
  | InvalidKeysetNameFormat Text
  -- ^ define-keyset name invalid format
  | CannotDefineKeysetOutsideNamespace
  -- ^ User attempted define a keyset outside of a namespace
  | NamespaceNotFound NamespaceName
  -- ^ Namespace not found in pactdb
  | NativeExecutionError NativeName Text
  -- ^ Native execution error, with reason
  | OperationIsLocalOnly NativeName
  -- ^ Native function is local-only
  | CannotApplyValueToNonClosure
  -- ^ Attempted to apply a non-closure
  | InvalidCustomKeysetPredicate Text
  -- ^ Invalid keyset predicate
  | HyperlaneError HyperlaneError
  -- ^ Hyperlane error
  | HyperlaneDecodeError HyperlaneDecodeError
  -- ^ Hyperlane decoding error
  | OwneraError OwneraError
  -- ^ Ownera error
  | ModuleAdminNotAcquired ModuleName
  -- ^ Module admin was needed for a particular operation, but has not been acquired.
  | UnknownException Text
  -- ^ An unknown exception was thrown and converted to text. Intentionally and crucially lazy.
  deriving (Eq, Show, Generic)

instance NFData EvalError


instance Pretty EvalError where
  pretty = \case
    ArrayOutOfBoundsException len ix ->
      Pretty.hsep
      [ "Array index out of bounds. Length"
      , Pretty.parens (pretty len) <> ","
      , "Index"
      , Pretty.parens (pretty ix)]
    ArithmeticException txt ->
      Pretty.hsep ["Arithmetic exception:", pretty txt]
    EnumerationError txt ->
      Pretty.hsep ["Enumeration error:", pretty txt]
    DecodeError txt ->
      Pretty.hsep ["Decoding error:", pretty txt]
    FloatingPointError txt ->
      Pretty.hsep ["Floating point error:", pretty txt]
    GasExceeded lim amt ->
      Pretty.hsep ["Gas limit", Pretty.dquotes (pretty lim) <> "exceeded:", pretty amt]
    InvariantFailure msg ->
      Pretty.hsep ["Fatal execution error, invariant violated:", pretty msg]
    NativeArgumentsError n tys ->
      Pretty.hsep ["Type error: failed to call native function", pretty n, "with argument(s) of type(s):", Pretty.commaSep tys]
    EvalError txt ->
      Pretty.hsep ["Program encountered an unhandled raised error:", pretty txt]
    YieldOutsideDefPact ->
      "Try to yield a value outside a running defpact execution"
    NoActiveDefPactExec ->
      "No active defpact execution in the environment"
    NoYieldInDefPactStep (DefPactStep step _ i _) ->
      Pretty.hsep ["No yield value found from previous pact step.", "Current step: " <> pretty step, "defpact Id: " <> pretty i]
    InvalidDefPactStepSupplied (DefPactStep step _ _ _) stepCount ->
      Pretty.hsep
      [ "DefPactStep does not match defpact properties:"
      , "requested: "<> pretty step
      , "step count:" <> pretty stepCount]
    DefPactIdMismatch reqId envId ->
      Pretty.hsep
      [ "Requested defpact id:", pretty reqId
      , "does not match context defpact id:", pretty envId
      ]
    CCDefPactContinuationError pactStep _ccExec _dbExec ->
      Pretty.hsep
      [ "Crosschain defpact continuation error:"
      , "defpact id:" <> pretty (_psStep pactStep)
      ]
    NestedDefPactParentRollbackMismatch pid rollback parentRollback ->
      Pretty.hsep
      [ "Nested defpact execution failed, parameter mismatch:"
      , "defpact id: " <> pretty pid
      , "Rollback: " <> pretty rollback
      , "Parent rollback:" <> pretty parentRollback
      ]
    NestedDefPactParentStepCountMismatch pid stepCount parentStepCount ->
      Pretty.hsep
      [ "Nested defpact execution failed, parameter mismatch:"
      , "PacId: " <> pretty pid
      , "step count: " <> pretty stepCount
      , "Parent step count: " <> pretty parentStepCount
      ]
    NoPreviousDefPactExecutionFound ps ->
      Pretty.hsep ["No previous defpact execution found for defpact id: ", pretty (_psDefPactId ps)]
    DefPactAlreadyCompleted ps -> Pretty.hsep
      [ "Requested defpact already completed: ", "defpact id:" <> pretty (_psDefPactId ps)]
    NestedDefPactNeverStarted ps -> Pretty.hsep
      ["Requested nested defpact never started:", "defpact id: " <> pretty (_psDefPactId ps)]
    NestedDefPactDoubleExecution ps -> Pretty.hsep
      ["Requested nested defpact double execution:", "defpact id: " <> pretty (_psDefPactId ps)]
    MultipleOrNestedDefPactExecFound pe -> Pretty.hsep
      ["defpact execution context already in the environment: ", "defpact id: " <> pretty (_peDefPactId pe)]
    DefPactStepHasNoRollback ps -> Pretty.hsep
      ["Step has no rollback:", "defpact id: " <> pretty (_psDefPactId ps)]
    DefPactStepNotInEnvironment -> "No DefPactStep in the environment"
    NoDefPactIdAndExecEnvSupplied -> "No defpact id or execution environment supplied"
    DefPactRollbackMismatch ps pe -> Pretty.hsep
      [ "Rollback mismatch in DefPactStep and defpact execution environment:"
      , "defpact id: " <> pretty (_psDefPactId ps)
      , "step rollback: " <> pretty (_psRollback ps)
      , "DefPactExec rollback: " <> pretty (_peStepHasRollback pe)
      ]
    DefPactStepMismatch ps pe -> Pretty.hsep
      [ "Step mismatch in DefPactStep and defpact execution environment:"
      , "defpact id: " <> pretty (_psDefPactId ps)
      , "step: " <> pretty (_psStep ps)
      , "DefPactExec step: " <> pretty (_peStep pe + 1)
      ]
    EnforcePactVersionFailure min' max' -> Pretty.hsep
      [ "Enforce pact-version failed:"
      , "Current Version: " <> pretty (V.showVersion PI.version)
      , "Minimum Version: " <> pretty (V.showVersion min')
      , maybe mempty (\v -> "Maximum Version: " <> pretty (V.showVersion v)) max'
      ]
    EnforcePactVersionParseFailure str -> Pretty.hsep
      [ "Enforce pact-version failed:"
      , "Could not parse " <> pretty str <> ", expect list of dot-separated integers"
      ]
    -- Todo: Fix each case
    InvalidManagedCap fqn ->
      "Install capability error: capability is not managed and cannot be installed:" <+> pretty (fqnToQualName fqn)
    CapNotInstalled cap ->
      "Capability not installed:" <+> pretty cap
    CapAlreadyInstalled cap ->
      "Capability already installed:" <+> pretty cap
    ModuleMemberDoesNotExist fqn ->
      "Module member does not exist" <+> pretty fqn
    NoSuchKeySet ksn ->
      "Cannot find keyset in database:" <+> pretty ksn
    CannotUpgradeInterface ifn ->
      "Interface cannot be upgraded:" <+> pretty ifn

    DbOpFailure dbe ->
      "Error during database operation:" <+> pretty dbe
    DynNameIsNotModRef n ->
      "Attempted to use" <+> pretty n <+> "as dynamic name, but it is not a modref"
    ModuleDoesNotExist m ->
      "Cannot find module:" <+> pretty m
    ExpectedModule mn ->
      "Expected module, found interface:" <+> pretty mn
    HashNotBlessed mn hs  ->
      "Execution aborted, hash not blessed for module" <+> pretty mn <> ":" <+> pretty hs
    CannotApplyPartialClosure ->
      "Attempted to apply a closure outside of native callsite"
    ClosureAppliedToTooManyArgs ->
      "Attempted to apply a closure to too many arguments"
    FormIllegalWithinDefcap msg ->
      "Form illegal within defcap" <+> pretty msg
    RunTimeTypecheckFailure argErr ty ->
      "Runtime typecheck failure, argument is" <+> pretty argErr <+> ", but expected type" <+> pretty ty
    NativeIsTopLevelOnly b ->
      "Top-level call used in module" <+> pretty b
    EventDoesNotMatchModule mn ->
      "Emitted event does not match module" <+> pretty mn
    InvalidEventCap fqn ->
      "Invalid event capability" <+> pretty fqn
    NestedDefpactsNotAdvanced dpid ->
      "Nested defpact not advanced" <+> pretty dpid
    ExpectedPactValue ->
      "Expected Pact Value, got closure or table reference"
    NotInDefPactExecution ->
      "Attempted to fetch defpact data, but currently not within defpact execution"
    RootNamespaceInstallError ->
      "Namespace installation error: cannot install in root namespace"
    PointNotOnCurve ->
      "Point lies outside of elliptic curve"
    YieldProvenanceDoesNotMatch received expected ->
      "Yield provenance does not match, received" <+> pretty received <> ", expected" <+> pretty expected
    MismatchingKeysetNamespace ns ->
      "Error defining keyset, namespace mismatch, expected " <> pretty ns
    RuntimeRecursionDetected qn ->
      "Recursion detected by the runtime. Recursing in function:" <+> pretty qn
    SPVVerificationFailure e ->
      "SPV verification failure:" <+> pretty e
    ExpectedBoolValue pv ->
      "expected bool value, got" <+> pretty pv
    UserGuardMustBeADefun qn dk ->
      "User guard closure" <+> pretty qn <+> "must be defun, got" <> pretty dk
    WriteValueDidNotMatchSchema (Schema _ sc) od ->
      "Attempted insert failed due to schema mismatch. Expected:" <+> pretty (ObjectData sc)
        <> ", received" <+> pretty od
    ObjectIsMissingField f b ->
      "Key" <+> dquotes (pretty f) <+> "not found in object:" <+> pretty b
    NativeExecutionError n msg ->
      "native execution failure," <+> pretty n <+> "failed with message:" <+> pretty msg
    ExpectedStringValue pv ->
      "expected string value, got:" <+> pretty pv
    ExpectedCapToken pv ->
      "expected capability token value, got:" <+> pretty pv
    InvalidKeysetFormat ks ->
      "Invalid keyset format:" <+> pretty ks
    InvalidKeysetNameFormat ksn ->
      "Invalid keyset name format:" <+> pretty ksn
    CannotDefineKeysetOutsideNamespace ->
      "Cannot define keyset outside of a namespace"
    NamespaceNotFound nsn ->
      "Namespace not found:" <+> pretty nsn
    ContinuationError msg ->
      "Continuation Error:" <+> pretty msg
    OperationIsLocalOnly n ->
      "Operation only permitted in local execution mode:" <+> pretty n
    CannotApplyValueToNonClosure ->
      "Cannot apply value to non-closure"
    InvalidCustomKeysetPredicate pn ->
      "Invalid custom predicate for keyset" <+> pretty pn
    HyperlaneError he -> "Hyperlane native error:" <+> pretty he
    HyperlaneDecodeError he -> "Hyperlane decode error:" <+> pretty he
    OwneraError oe -> "Ownera native error:" <+> pretty oe
    ModuleAdminNotAcquired mn ->
      "Module admin necessary for operation but has not been acquired:" <> pretty mn
    UnknownException msg ->
      "Unknown exception: " <> pretty msg

-- | Errors meant to be raised
--   internally by a PactDb implementation
data DbOpError
  = WriteError
  | RowReadDecodeFailure Text
  | RowFoundError TableName RowKey
  | NoRowFound TableName RowKey
  | NoSuchTable TableName
  | TableAlreadyExists TableName
  | TxAlreadyBegun Text
  | NotInTx Text
  | OpDisallowed
  | MultipleRowsReturnedFromSingleWrite
  deriving (Show, Eq, Typeable, Generic)

instance NFData DbOpError

instance Pretty DbOpError where
  pretty = \case
    WriteError ->
      "Error found while writing value"
    RowReadDecodeFailure rk ->
      "Failed to deserialize but found value at key:" <> pretty rk
    RowFoundError tn rk ->
      "Value already found while in Insert mode in table" <+> pretty tn <+> "at key" <+> dquotes (pretty rk)
    NoRowFound tn rk ->
      "No row found during update in table" <+> pretty tn <+> "at key" <+> pretty rk
    NoSuchTable tn ->
      "Table" <+> pretty tn <+> "not found"
    TableAlreadyExists tn ->
      "Table" <+> pretty tn <+> "already exists"
    TxAlreadyBegun tx ->
      "Attempted to begin tx" <+> dquotes (pretty tx) <> ", but a tx already has been initiated"
    NotInTx cmd ->
      "No Transaction currently in progress, cannot execute " <> dquotes (pretty cmd)
    OpDisallowed ->
      "Operation disallowed in read-only or sys-only mode"
    MultipleRowsReturnedFromSingleWrite ->
      "Multiple rows returned from single write"



data UserRecoverableError
  = UserEnforceError Text
  -- ^ Errors produced by `enforce` or `enforceOne`
  | OneShotCapAlreadyUsed
  -- ^ A one-shot capability has already been fired
  | CapabilityNotGranted (CapToken QualifiedName PactValue)
  -- ^ Capability not granted in require-cap
  | NoSuchObjectInDb TableName RowKey
  -- ^ Did not find an object in the specified table for some rowkey
  | KeysetPredicateFailure KSPredicate (Set PublicKeyText)
  -- ^ Keyset Predicate failed for some reason
  | CapabilityPactGuardInvalidPactId DefPactId DefPactId
  -- ^ Mismatching pact ID's in capability guard
  | EnvReadFunctionFailure NativeName
  -- ^ read-* function failure
  | VerifierFailure VerifierName Text
  -- ^ Verifier failure
  | CapabilityGuardNotAcquired (CapabilityGuard QualifiedName PactValue)
  deriving (Show, Eq, Generic, Typeable)

instance NFData UserRecoverableError

instance Pretty UserRecoverableError where
  pretty = \case
    UserEnforceError t -> pretty t
    OneShotCapAlreadyUsed -> "One-shot managed capability used more than once"
    CapabilityNotGranted ct ->
      "require-capability: not granted:" <+> parens (pretty (_ctName ct))
    NoSuchObjectInDb tn (RowKey rk) ->
      "No value found in table" <+> pretty tn <+> "for key:" <+> pretty rk
    KeysetPredicateFailure ksPred kskeys ->
      "Keyset failure (" <> pretty ksPred <> "): "
               <> pretty (map (elide . renderPublicKeyText) $ S.toList kskeys)
      where
      elide pk = T.take 8 pk <> "..."
    CapabilityPactGuardInvalidPactId currPid pgId ->
      "Capability pact guard failed: invalid pact id, expected" <+> pretty pgId <+> "got"
        <+> pretty currPid
    EnvReadFunctionFailure desc ->
      pretty desc <+> "failure"
    VerifierFailure (VerifierName verif) msg ->
      "Verifier failure" <+> pretty verif <> ":" <+> pretty msg
    CapabilityGuardNotAcquired cg ->
      "Capability not acquired:" <+> pretty cg

data HyperlaneError
  = HyperlaneErrorFailedToFindKey Field
    -- ^ An expected key was not found.
  | HyperlaneErrorNumberOutOfBounds Field
    -- ^ The number at this field was outside of the expected bounds of its
    -- type.
  | HyperlaneErrorBadHexPrefix Field
    -- ^ Hex textual fields (usually ETH addresses) must be prefixed with "0x"
  | HyperlaneErrorInvalidBase64 Field
    -- ^ Invalid base64 text field.
  | HyperlaneErrorIncorrectSize Field Int Int
    -- ^ Invalid Hex. We discard error messages from base16-bytestring to
  | HyperlaneErrorInvalidChainId Text
    -- ^ Invalid chain id.
  deriving (Eq, Show, Generic)

instance NFData HyperlaneError

instance Pretty HyperlaneError where
  pretty = \case
    HyperlaneErrorFailedToFindKey key -> "Failed to find key in object: " <> pretty key
    HyperlaneErrorNumberOutOfBounds key -> "Object key " <> pretty key <> " was out of bounds"
    HyperlaneErrorBadHexPrefix key -> "Missing 0x prefix on field " <> pretty key
    HyperlaneErrorInvalidBase64 key -> "Invalid base64 encoding on field " <> pretty key
    HyperlaneErrorIncorrectSize key expected actual ->
      "Incorrect binary data size " <> pretty key <> ". Expected: " <> pretty expected <> ", but got " <> pretty actual
    HyperlaneErrorInvalidChainId msg -> "Failed to decode chainId: " <> pretty msg

data HyperlaneDecodeError
  = HyperlaneDecodeErrorBase64
    -- ^ We discard the error message in this case to maintain error message
    --   equality with the original implementation - otherwise this would have a
    --   string in it
  | HyperlaneDecodeErrorInternal String
    -- ^ Decoding error that our own code threw, not `binary`
  | HyperlaneDecodeErrorBinary
    -- ^ We encountered an error not thrown by us but by `binary`. We discard
    --   the error message to avoid potentially forking behaviour introduced
    --   by a library update.
  | HyperlaneDecodeErrorParseRecipient
    -- ^ Failed to parse the Recipient into a Guard
  deriving (Eq, Show, Generic)

instance NFData HyperlaneDecodeError

instance Pretty HyperlaneDecodeError where
  pretty = \case
    HyperlaneDecodeErrorBase64 -> "Failed to base64-decode token message"
    HyperlaneDecodeErrorInternal errmsg -> "Decoding error: " <> pretty errmsg
    HyperlaneDecodeErrorBinary -> "Decoding error: binary decoding failed"
    HyperlaneDecodeErrorParseRecipient -> "Could not parse recipient into a guard"




data OwneraError
  = OwneraErrorFailedToFindKey Field
    -- ^ An expected key was not found.
  | OwneraErrorFailedToFindHashGroup Field
    -- ^ An expected key was not found.
  deriving (Eq, Show, Generic)

instance NFData OwneraError

instance Pretty OwneraError where
  pretty = \case
    OwneraErrorFailedToFindKey key -> "Failed to find key in object: " <> pretty key
    OwneraErrorFailedToFindHashGroup key -> "Failed to find hash group in object: " <> pretty key
    


data VerifierError
  = VerifierError { _verifierError :: Text }
  deriving (Eq, Ord, Show, Generic)

instance NFData VerifierError

instance Pretty VerifierError where
  pretty (VerifierError v) =
    "Error during verifier execution: " <> pretty v

-- | Our main ADT for errors.
--   Our erros on chain (and in the repl) are separated into:
--   - Errors from lexing
--   - Errors from parsing
--   - Errors from desugaring + renaming
--   - Unrecoverable errors resulting from pact execution
--   - Recoverable, but unhandled errors emitted by user code during pact execution
--   - Errors from running a verifier plugin (Chainweb)
data PactError info
  = PELexerError LexerError info
  | PEParseError ParseError info
  | PEDesugarError DesugarError info
  | PEExecutionError EvalError [StackFrame info] info
  | PEUserRecoverableError UserRecoverableError [StackFrame info] info
  | PEVerifierError VerifierError info
  deriving (Eq, Show, Functor, Generic)

-- | A pact error origin is either:
--   - A function call where it occurred
--   - A top level statement
data PactErrorOrigin
  = TopLevelErrorOrigin
  | FunctionErrorOrigin FullyQualifiedName
  deriving (Eq, Show)

instance J.Encode PactErrorOrigin where
  build = \case
    TopLevelErrorOrigin -> J.text "<toplevel>"
    FunctionErrorOrigin fqn -> J.build (renderFullyQualName fqn)

instance JD.FromJSON PactErrorOrigin where
  parseJSON = JD.withText "PactErrorOrigin" $ \case
    "<toplevel>" -> pure TopLevelErrorOrigin
    t -> case parseFullyQualifiedName t of
      Just fqn -> pure (FunctionErrorOrigin fqn)
      Nothing -> fail "failure parsing pact error origins"

-- | A `LocatedErrorInfo` is a pair of
--   an error origin (so a function call or a top level statement)
--   as well as the line or span info from the source text
data LocatedErrorInfo info
  = LocatedErrorInfo
  { _leiOrigin :: PactErrorOrigin
  , _leiInfo :: info
  } deriving (Eq, Show, Functor, Foldable, Traversable)

instance Default info => Default (LocatedErrorInfo info) where
  def = LocatedErrorInfo TopLevelErrorOrigin def

locatePactErrorInfo :: PactError info -> PactError (LocatedErrorInfo info)
locatePactErrorInfo pe =
  case viewErrorStack pe of
    sf:_ -> fmap (LocatedErrorInfo (FunctionErrorOrigin (_sfName sf))) pe
    [] -> fmap (LocatedErrorInfo TopLevelErrorOrigin) pe

instance J.Encode info => J.Encode (LocatedErrorInfo info) where
  build loc = J.object
    [ "errorOrigin" J..= _leiOrigin loc
    , "errorInfo" J..= _leiInfo loc
    ]

instance JD.FromJSON info => JD.FromJSON (LocatedErrorInfo info) where
  parseJSON = JD.withObject "LocatedErrorInfo" $ \o ->
    LocatedErrorInfo
      <$> o JD..: "errorOrigin"
      <*> o JD..: "errorInfo"

instance NFData info => NFData (PactError info)

instance Pretty (PactError info) where
  pretty = \case
    PELexerError e _ -> pretty e
    PEParseError e _ -> pretty e
    PEDesugarError e _ -> pretty e
    PEExecutionError e _ _ ->
      pretty e
    PEUserRecoverableError e _ _ ->
      pretty e
    PEVerifierError e _ -> pretty e

peInfo :: Lens (PactError info) (PactError info) info info
peInfo f = \case
  PELexerError le info ->
    PELexerError le <$> f info
  PEParseError pe info ->
    PEParseError pe <$> f info
  PEDesugarError de info ->
    PEDesugarError de <$> f info
  PEExecutionError ee stack info ->
    PEExecutionError ee stack <$> f info
  PEUserRecoverableError ee stack info ->
    PEUserRecoverableError ee stack <$> f info
  PEVerifierError err info ->
    PEVerifierError err <$> f info

viewErrorStack :: PactError info -> [StackFrame info]
viewErrorStack = \case
  PEExecutionError _ stack _ -> stack
  PEUserRecoverableError _ stack _ -> stack
  _ -> []

deriveConstrInfo ''VerifierError
deriveConstrInfo ''LexerError
deriveConstrInfo ''ParseError
deriveConstrInfo ''DesugarError
deriveConstrInfo ''EvalError
deriveConstrInfo ''UserRecoverableError
deriveConstrInfo ''PactError


---------------------------------------------------------------------------------------
-- Error code functions start here
---------------------------------------------------------------------------------------

newtype BoundedText (k :: Nat)
  = BoundedText {_boundedText ::  Text }
  deriving newtype (Eq, Show, JD.FromJSON, J.Encode)

instance KnownNat k => IsString (BoundedText k) where
  fromString = ensureBound . T.pack

type PactErrorMsgSize = 256

{-
  Note [Bounded Text Errors]

  In pact-5 we have error codes, but it would be too much of a service downgrade
  to no longer have any sort of error message, so we have returned them to pact 5, on the
  condition that they are bounded. We HAVE TO ENSURE we don't spend too much time computing
  the actual texts themselves (which the functions tagged with this note do).

  Any function tagged with this note needs to be _extremely_ careful
  about how it generates error messages.

  The following a set of safe rules to ensure that we can still give
  good enough errors to users, while avoiding issues with potentially
  spending too much time serializing errors:
  - _always_ use `thsep` to concatenate sets of texts when in doubt. It ensures that
    the `PactErrorMsgSize` bound is maintained
  - No uses of `show` inside of the functions. Make a helper if necessary that calls `show`
    when it is safe to do so
  - Do _not_ even attempt to pretty print a pact value, unless absolutely necessary,
    and even then, side eye it many times. PactValue inputs are generally user-controlled.
  - Do not use `renderCompactText` or any functions that come from the `Pretty` module.
    this is extremely unsafe, because `Pretty` should not care at all about this very specific
    constraint
  - Any user controlled text, if it comes from a `RowKey` or any other input, should be
    `abbrevText`'d (how much to abbrev at your discretion).
  - Strings that come from inputs which we control (double check to be safe) are fine to not
    abbrev. They're usually small enough for us not to care.
-}

ensureBound :: forall k. KnownNat k => Text -> BoundedText k
ensureBound msg = BoundedText (T.take (fromIntegral (natVal (Proxy @k))) msg)

-- | NOTE: Do _not_ change this function post mainnet release just to improve an error.
--  This will fork the chain, these messages will make it into outputs.
--  See [Bounded Text Errors]
dbOpErrorToBoundedText' :: DbOpError -> Text
dbOpErrorToBoundedText' = \case
    WriteError ->
      "Database error: Writing failed because of an error."
    RowReadDecodeFailure rk ->
      thsep ["Deserialization failed for the value at key:", abbrevText 10 rk]
    RowFoundError tn rk ->
      thsep
        ["Insert failed because the value already exists in the table:"
        , renderTableName tn, "at key"
        , tdquotes $ abbrevRowKey rk]
    NoRowFound tn rk ->
      thsep
        ["Update failed because no row was found in table:"
        , renderTableName tn
        , "at key"
        , tdquotes $ abbrevRowKey rk ]
    NoSuchTable tn ->
      thsep ["Insert/Update failed because table ", renderTableName tn, "was not found."]
    TableAlreadyExists tn ->
      thsep ["Creation of user table", renderTableName tn, "failed because the table already exists."]
    TxAlreadyBegun _ ->
      "Starting a new transaction failed because a transaction has already been initiated."
    NotInTx cmd ->
      thsep ["Cannot execute", cmd, "because no transaction is currently in progress."]
    OpDisallowed ->
      "Operation is not allowed in read-only or system-only mode."
    MultipleRowsReturnedFromSingleWrite ->
      "Multiple rows were returned from a single write."

-- | NOTE: Do _not_ change this function post mainnet release just to improve an error.
--  This will fork the chain, these messages will make it into outputs.
--  See [Bounded Text Errors]
invariantErrorToBoundedText' :: InvariantError -> Text
invariantErrorToBoundedText' = \case
  InvariantInvalidDefKind dk t ->
    thsep ["Invalid def kind, received", renderDefKind dk, t]
  -- Note: the following 5 cases use `renderFullyQualName` and not
  -- tFqn because we actually care to see the full module hash of the function
  -- in question.
  -- These errors should never happen on chain, when they do, there's a problem in the
  -- compiler and it's better we see it
  InvariantDefConstNotEvaluated fqn ->
    thsep ["Defconst was not evaluated prior to execution:", renderFullyQualName fqn]
  InvariantExpectedDefCap fqn ->
    thsep ["Expected a defcap for free variable", renderFullyQualName fqn]
  InvariantExpectedDefun fqn ->
    thsep ["Expected a defun for free variable", renderFullyQualName fqn]
  InvariantExpectedDefPact fqn ->
    thsep ["Expected a defpact for free variable", renderFullyQualName fqn]
  InvariantUnboundFreeVariable fqn ->
    thsep ["Unbound free variable", renderFullyQualName fqn]
  InvariantInvalidBoundVariable v ->
    thsep ["Invalid bound or free variable:", v]
  InvariantMalformedDefun dfn ->
    thsep ["Malformed defun: Body is not a lambda", renderFullyQualName dfn]
  InvariantPactExecNotInEnv _ ->
    "No pact exec found in env"
  InvariantPactStepNotInEnv _ ->
    "No pact step found in env"
  InvariantInvalidManagedCapIndex i fqn ->
    thsep ["Invalid managed cap argument index", tInt i, "for function", tFqn fqn]
  InvariantArgLengthMismatch fqn expected got ->
    thsep
      ["Argument length mismatch for", renderFullyQualName fqn, ", expected"
      , tInt expected <> ","
      , "got"
      , tInt got]
  InvariantInvalidManagedCapKind msg ->
    thsep ["Invalid managed cap kind", msg]
  InvariantNoSuchKeyInTable tbl (RowKey rk) ->
    thsep
      ["No such key"
      , rk
      , "in table"
      , renderTableName tbl]
  InvariantEmptyCapStackFailure ->
    "Attempted to pop or manipulate the capstack, but it was found to be empty"

hyperlaneErrorToBoundedText' :: HyperlaneError -> Text
hyperlaneErrorToBoundedText' = \case
  HyperlaneErrorFailedToFindKey key ->
    thsep ["Failed to find key in object:", _field key]
  HyperlaneErrorNumberOutOfBounds key ->
    thsep ["Object key", _field key, "was out of bounds."]
  HyperlaneErrorBadHexPrefix key ->
    thsep ["Missing 0x prefix on field:", _field key]
  HyperlaneErrorInvalidBase64 key ->
    thsep ["Invalid base64 encoding on field:", _field key]
  HyperlaneErrorIncorrectSize key expected actual ->
    thsep
      ["Incorrect binary data size."
      , _field key <> ". Expected:"
      , tInt expected <> ", but got:"
      , tInt actual]
  -- Library decoding error: do not serialise
  HyperlaneErrorInvalidChainId _ ->
    thsep ["Failed to decode chainId."]

hyperlaneDecodeErrorToBoundedText' :: HyperlaneDecodeError -> Text
hyperlaneDecodeErrorToBoundedText' = \case
  HyperlaneDecodeErrorBase64 -> "Failed to base64-decode token message."
  -- Library error, do not serialise
  HyperlaneDecodeErrorInternal _ ->
    "Hyperlane Native Internal Decoding error"
  HyperlaneDecodeErrorBinary -> "Decoding error: binary decoding failed."
  HyperlaneDecodeErrorParseRecipient -> "Could not parse recipient into a guard"

-- | NOTE: Do _not_ change this function post mainnet release just to improve an error.
--  This will fork the chain, these messages will make it into outputs.
--  See [Bounded Text Errors]
evalErrorToBoundedText :: EvalError -> BoundedText PactErrorMsgSize
evalErrorToBoundedText = mkBoundedText . \case
  ArrayOutOfBoundsException len ix ->
    thsep
    [ "Array index out of bounds. Length:"
    , tInt len <> ","
    , "Index:"
    , tInt ix]
  ArithmeticException txt ->
    thsep ["Arithmetic exception:", txt]
  EnumerationError txt ->
    thsep ["Enumeration error:", txt]
  DecodeError txt ->
    thsep ["Decoding error:", txt]
  FloatingPointError txt ->
    thsep ["Floating point error:", txt]
  GasExceeded lim gas ->
    thsep ["Gas limit", tdquotes (tGasLimit lim) <> "exceeded:", tGas gas]
  InvariantFailure msg ->
    thsep
      [ "Execution Invariant error, please report this to the Pact team @kadena:"
      , invariantErrorToBoundedText' msg]
  NativeArgumentsError (NativeName n) tys ->
    thsep $
      ["Type error: failed to call native function", n <> ",", "with argument(s) of type(s):"]
      ++ tCommaSep (renderArgTypeError <$> tys)
  EvalError txt ->
    thsep ["Program encountered an unhandled error:", txt]
  YieldOutsideDefPact ->
    "Failed to yield a value outside of a running defpact execution."
  NoActiveDefPactExec ->
    "No active defpact execution found in the environment."
  NoYieldInDefPactStep (DefPactStep step _ (DefPactId dpid) _) ->
    thsep ["No yield value found from the previous defpact step.", "Current step:", tInt step, "defpact id:", dpid]
  InvalidDefPactStepSupplied (DefPactStep step _ _ _) stepCount ->
    thsep
    [ "Step does not match defpact properties:"
    , "Requested:", tInt step
    , "Step count:", tInt stepCount]
  DefPactIdMismatch reqId envId ->
    thsep
    [ "Requested defpact id:", tDpId reqId
    , "does not match context defpact id:", tDpId envId
    ]
  CCDefPactContinuationError pactStep _ccExec _dbExec ->
    thsep
    [ "Cross-chain defpact continuation error:"
    , "defpact id:", (tDpId (_psDefPactId pactStep))
    ]
  NestedDefPactParentRollbackMismatch pid rollback parentRollback ->
    thsep
    [ "Nested defpact execution failed for defpact id:", tDpId pid
    , "Parameter mismatch."
    , "Rollback:", tBool rollback
    , "Parent rollback:", tBool parentRollback
    ]
  NestedDefPactParentStepCountMismatch pid stepCount parentStepCount ->
    thsep
    [ "Nested defpact execution failed for defpact id:", tDpId pid
    , "Parameter mismatch."
    , "Step count:", T.pack (show stepCount)
    , "Parent step count:", T.pack (show parentStepCount)
    ]
  NoPreviousDefPactExecutionFound ps ->
    thsep ["No previous defpact execution found for defpact id: ", tDpId (_psDefPactId ps)]
  DefPactAlreadyCompleted ps -> thsep
    ["Requested defpact execution already completed for defpact id:", tDpId (_psDefPactId ps)]
  NestedDefPactNeverStarted ps -> thsep
    ["Requested nested defpact execution never started for defpact id:", tDpId(_psDefPactId ps)]
  NestedDefPactDoubleExecution ps -> thsep
    ["Requested nested defpact double execution failed for defpact id:" , tDpId (_psDefPactId ps)]
  MultipleOrNestedDefPactExecFound pe -> thsep
    ["Another defpact execution context is already running in the environment for defpact id:", tDpId (_peDefPactId pe)]
  DefPactStepHasNoRollback ps -> thsep
    ["Step has no rollback in defpact id:", tDpId (_psDefPactId ps)]
  DefPactStepNotInEnvironment -> "No defpact step found in the environment."
  NoDefPactIdAndExecEnvSupplied -> "No defpact id or execution environment supplied."
  DefPactRollbackMismatch ps pe -> thsep
    [ "Rollback mismatch in defpact step and defpact execution environment for defpact id:", tDpId (_psDefPactId ps)
    , "Step rollback:", tBool (_psRollback ps)
    , "Execution environment rollback:", tBool (_peStepHasRollback pe)
    ]
  DefPactStepMismatch ps pe -> thsep
    [ "Step mismatch in DefPactStep and defpact execution environment for defpact id:", tDpId (_psDefPactId ps)
    , "Step:", tInt (_psStep ps)
    , "Execution environment step:", tInt (_peStep pe + 1)
    ]
  EnforcePactVersionFailure min' max' -> thsep
    [ "Enforcement of Pact version failed."
    , "Current version:", T.pack (V.showVersion PI.version)
    , "Minimum version:", T.pack (V.showVersion min')
    , maybe mempty (\v -> "Maximum version: " <> T.pack (V.showVersion v)) max'
    ]
  EnforcePactVersionParseFailure str -> thsep
    [ "Enforcement of Pact version failed:"
    , "Could not parse", str, ", expect a list of dot-separated integers."
    ]
  InvalidManagedCap fqn ->
    thsep ["Install capability failed. Capability is not declared as a managed capability and cannot be installed.", tFqn fqn]
  CapNotInstalled cap ->
    thsep
      ["Capability"
      , renderQualName (_ctName cap)
      , "was not installed."
      , "Check the sigs field or the arguments to verify that the capability is specified correctly."]
  CapAlreadyInstalled cap ->
    thsep ["Capability"
          , renderQualName (_ctName cap)
          , "already installed."]
  ModuleMemberDoesNotExist fqn ->
    thsep ["Module member does not exist:", tFqn fqn]
  NoSuchKeySet ksn ->
    thsep ["Cannot find keyset in the database:", renderKeySetName ksn]
  CannotUpgradeInterface ifn ->
    thsep ["Interface cannot be upgraded:", renderModuleName ifn]
  DbOpFailure dbe ->
    thsep ["Error during database operation:", dbOpErrorToBoundedText' dbe]
  DynNameIsNotModRef n ->
    thsep ["Attempt to use", abbrevText 25 n, "as a dynamic name failed because it is not a valid modref."]
  ModuleDoesNotExist m ->
    thsep ["Cannot find module:", renderModuleName m]
  ExpectedModule mn ->
    thsep ["Expected module, found interface:", renderModuleName mn]
  HashNotBlessed mn hs  ->
    thsep
      ["Execution aborted, hash not blessed for module:"
      , renderModuleName mn
      , "with hash"
      , moduleHashToText hs]
  CannotApplyPartialClosure ->
    "Failed to apply a partial closure outside of the native callsite."
  ClosureAppliedToTooManyArgs ->
    "Failed to apply a function or closure because there were too many arguments."
  FormIllegalWithinDefcap msg ->
    thsep ["Form not allowed within a defcap declaration.", msg]
  RunTimeTypecheckFailure argErr ty ->
    thsep
      ["Type check failed. The argument is "
      , renderArgTypeError argErr
      , "but the expected type is "
      , renderArgTypeError (typeToArgTypeError ty)]
  NativeIsTopLevelOnly b ->
    thsep ["Top-level call used in module:", _natName b]
  EventDoesNotMatchModule mn ->
    thsep ["Emitted event does not match module:", renderModuleName mn]
  InvalidEventCap fqn ->
    thsep ["Invalid event capability:", tFqn fqn]
  NestedDefpactsNotAdvanced dpid ->
    thsep ["Nested defpact execution failed to advanced for defpact id:", tDpId dpid]
  ExpectedPactValue ->
    "Expected a Pact value, but got a closure or table reference."
  NotInDefPactExecution ->
    "Attempt to fetch defpact data failed because there's no defpact currently being executed."
  RootNamespaceInstallError ->
    "Namespace installation failed. Cannot install modules in the root namespace."
  PointNotOnCurve ->
    "Point lies outside of the elliptic curve."
  YieldProvenanceDoesNotMatch received expected ->
    thsep
      (["Yield provenance does not match, received:"
      , renderProvenance received
      , ", expected:"
      ] ++ (renderProvenance <$> expected))
  MismatchingKeysetNamespace ns ->
    thsep
    ["Defining keyset failed because of a namespace mismatch. Expected namespace:", abbrevText 10 (_namespaceName ns)]
  RuntimeRecursionDetected qn ->
    thsep ["Recursion detected by the runtime in function:", renderQualName qn]
  SPVVerificationFailure e ->
    thsep ["SPV verification failure:", e]
  ExpectedBoolValue pv ->
    thsep
      [ "Expected Boolean value, but got:"
      , renderPvAsArgType pv]
  UserGuardMustBeADefun qn dk ->
    thsep
      [ "User guard closure"
      , renderQualName qn
      , "must be in a defun declaration, but got:"
      , renderDefKind dk]
  WriteValueDidNotMatchSchema (Schema qn _) _ ->
    thsep
      ["Insert failed because of a schema mismatch with", renderQualName qn]
  ObjectIsMissingField f _ ->
    thsep
      [ "Key"
      , abbrevText 10 (_field f)
      , "not found in object."]
  NativeExecutionError n msg ->
    thsep
      [ "Native execution failed for", _natName n
      , "with the message:"
      , msg] -- Note: these strings are controlled by us.
  ExpectedStringValue pv ->
    thsep
    ["Expected string value, but got:", renderPvAsArgType pv]
  ExpectedCapToken pv ->
    thsep ["Expected capability token value, but got:", renderPvAsArgType pv]
  InvalidKeysetFormat _ ->
    "Invalid keyset format. Check that keys have the right length and signature scheme."
  InvalidKeysetNameFormat ksn ->
    thsep ["Invalid keyset name format:", abbrevText 20 ksn]
  CannotDefineKeysetOutsideNamespace ->
    "Cannot define keyset outside of a namespace."
  NamespaceNotFound nsn ->
    thsep ["Namespace not found:", abbrevText 10 (_namespaceName nsn)]
  ContinuationError msg ->
    thsep ["Continuation error:", msg]
  OperationIsLocalOnly n ->
    thsep ["Operation only permitted in local execution mode:", _natName n]
  CannotApplyValueToNonClosure ->
    "Cannot apply value to a non-closure."
  InvalidCustomKeysetPredicate pn ->
    thsep ["Invalid custom predicate for keyset:", abbrevText 20 pn]
  HyperlaneError he ->
    thsep ["Hyperlane native error:", hyperlaneErrorToBoundedText' he]
  HyperlaneDecodeError he ->
    thsep ["Hyperlane decode error:", hyperlaneDecodeErrorToBoundedText' he]
  OwneraError _ ->
    thsep ["Ownera native error:", "todo"]
  ModuleAdminNotAcquired mn ->
    thsep
      ["Module admin is necessary for operation but has not been acquired:", renderModuleName mn]
  -- Maybe library dependent, do not serialise
  UnknownException _ ->
    thsep ["Unknown exception"]

-- | NOTE: Do _not_ change this function post mainnet release just to improve an error.
--  This will fork the chain, these messages will make it into outputs.
--  See [Bounded Text Errors]
userRecoverableErrorToBoundedText :: UserRecoverableError -> BoundedText PactErrorMsgSize
userRecoverableErrorToBoundedText = mkBoundedText . \case
  UserEnforceError t -> t
  OneShotCapAlreadyUsed -> "Managed capabilities can only be used once."
  CapabilityNotGranted ct ->
    thsep
      ["The required capability has not been granted:", renderQualName(_ctName ct)]
  NoSuchObjectInDb tn rk ->
    thsep
      ["No value found in table"
      , renderTableName tn
      , "for key:"
      , abbrevRowKey rk]
  KeysetPredicateFailure ksPred kskeys ->
    thsep
      (["Keyset failure ("
      , predicateToText ksPred
      , "): "] ++ tCommaSep (fmap (abbrevText 10 . renderPublicKeyText) $ S.toList kskeys))
  CapabilityPactGuardInvalidPactId currPid pgId ->
    thsep
      ["Pact guard failed because of an invalid pact id. Expected:"
      , tDpId pgId <> ","
      , "but got:"
      , tDpId currPid]
  EnvReadFunctionFailure desc ->
    thsep [_natName desc, "failed. Invalid format or missing key."]
  VerifierFailure (VerifierName verif) msg ->
    thsep ["Verifier", verif, "failed with the message:", msg]
  CapabilityGuardNotAcquired cg ->
    thsep ["Capability not acquired:", renderQualName (_cgName cg)]

-- | NOTE: Do _not_ change this function post mainnet release just to improve an error.
--  This will fork the chain, these messages will make it into outputs.
--  See [Bounded Text Errors]
desugarErrorToBoundedText :: DesugarError -> BoundedText PactErrorMsgSize
desugarErrorToBoundedText = mkBoundedText . \case
    UnboundTermVariable t ->
      thsep ["Unbound variable:", t]
    UnboundTypeVariable t ->
      thsep ["Unbound type variable:", t]
    InvalidCapabilityReference t ->
      thsep ["Variable or function used in special form is not a capability:", t]
    NoSuchModuleMember mn txt ->
      thsep ["Module", renderModuleName mn, "has no such member:", txt]
    NoSuchModule mn ->
      thsep ["Cannot find module: ", renderModuleName mn]
    NoSuchInterface mn ->
      thsep ["Cannot find interface: ", renderModuleName mn]
    NotAllowedWithinDefcap dc ->
      thsep [dc, "form not allowed within a defcap declaration."]
    NotAllowedOutsideModule txt ->
      thsep [txt, "not allowed outside of a module."]
    ImplementationError mn1 mn2 defn ->
      thsep [ "Module"
            , renderModuleName mn1
            , "does not correctly implement the function"
            , defn
            , "from the interface:"
            , renderModuleName mn2]
    RecursionDetected mn txts ->
      thsep $
      ["Recursive cycle detected in module"
      , renderModuleName mn
      , "in the following functions:"] ++ tCommaSep txts
    InvalidGovernanceRef gov ->
      thsep ["Invalid governance. Must be a 0-argument defcap or a keyset. Found:", renderQualName gov]
    InvalidDefInTermVariable n ->
      thsep ["Invalid definition in variable position:", n]
    InvalidModuleReference mn ->
      thsep ["Attempt to use an interface as a module reference failed:", renderModuleName mn]
    EmptyBindingBody -> "Bind expression lacks an accompanying body."
    LastStepWithRollback mn ->
      thsep ["Rollbacks aren't allowed on the last step in:", renderQualName mn]
    ExpectedFreeVariable t ->
      thsep ["Expected free variable in expression, found locally bound variable: ", t]
    -- Todo: pretty these
    InvalidManagedArg arg ->
      thsep ["Invalid managed argument. No argument with the name:", arg]
    NotImplemented mn ifn ifdn ->
      thsep
        ["Interface member not implemented. Module"
        , renderModuleName mn
        , "does not implement interface"
        , renderModuleName ifn
        , "member:"
        , ifdn]
    InvalidImports mn imps ->
      thsep $
        ["Invalid imports. Module or interface"
        , renderModuleName mn
        , "does not implement the following members:"] ++ tCommaSep imps
    InvalidImportModuleHash mn mh ->
      thsep
        ["Import error for module"
        , renderModuleName mn
        , ". The module hash has not been blessed:"
        , moduleHashToText mh]
    InvalidSyntax msg ->
      thsep ["Desugar syntax failure:", msg]
    InvalidDefInSchemaPosition n ->
      thsep ["definition in defschema position:", n, "is not a valid schema."]
    InvalidDynamicInvoke (DynamicName dnName dnCall) ->
      thsep ["Invalid dynamic call:", dnName <> "::" <> dnCall]
    DuplicateDefinition qn ->
      thsep ["Duplicate definition:", renderQualName qn]
    InvalidBlessedHash hs ->
      thsep ["Invalid blessed hash, incorrect format:", hs]

-- | NOTE: Do _not_ change this function post mainnet release just to improve an error.
--  This will fork the chain, these messages will make it into outputs.
--  See [Bounded Text Errors]
parseErrorToBoundedText :: ParseError -> BoundedText PactErrorMsgSize
parseErrorToBoundedText = mkBoundedText . \case
  ParsingError e ->
    thsep [pErr, "Expected:", e]
  TooManyCloseParens e ->
    thsep [pErr, "Too many closing parentheses. Remaining tokens:", e]
  UnexpectedInput e ->
    thsep [pErr, "Unexpected input after expression. Remaining tokens:", e]
  PrecisionOverflowError i ->
    thsep [pErr, "Precision overflow (>=255 decimal places): ", tInt i, "decimals"]
  InvalidBaseType txt ->
    thsep [pErr, "No such type:", txt]
  where
  pErr = "ParseError:"

lexerErrorToBoundedText :: LexerError -> BoundedText PactErrorMsgSize
lexerErrorToBoundedText = mkBoundedText . \case
  LexicalError c1 c2 ->
    thsep ["Encountered an unexpected character", tdquotes (T.singleton c1) <> ".", "Last seen", tdquotes (T.singleton c2)]
  StringLiteralError te ->
    thsep ["String literal parsing error: ", te]
  OutOfInputError c ->
    thsep ["Ran out of input before finding a lexeme. Last character seen: ", tdquotes (T.singleton c)]


-------------------------------------------------------------------------------------------
-- Utility functions for text conversion
-------------------------------------------------------------------------------------------
thsep :: [Text] -> Text
thsep = concatBounded (fromIntegral (natVal (Proxy @PactErrorMsgSize))) . intersperse " "
tdquotes :: Text -> Text
tdquotes x = T.concat ["\"", x, "\""]
tInt :: Int -> Text
tInt = T.pack . show
tBool :: Bool -> Text
tBool = T.pack . show
tFqn :: FullyQualifiedName -> Text
tFqn = renderQualName . fqnToQualName
tDpId :: DefPactId -> Text
tDpId = abbrevText 20 . _defPactId
tGas :: Gas -> Text
tGas (Gas w) = T.pack (show w)
tGasLimit :: GasLimit -> Text
tGasLimit (GasLimit g) = tGas g
tPunctuate :: Text -> [Text] -> [Text]
tPunctuate p = go
  where
  go [] = []
  go [t] = [t]
  go (t:ts) = (t <> p) : go ts

tCommaSep :: [Text] -> [Text]
tCommaSep = tPunctuate ","

abbrevText :: Int -> Text -> Text
abbrevText lim k
  | T.length k <= lim = k
  | otherwise = T.concat [T.take lim k, "..."]

abbrevRowKey :: RowKey -> Text
abbrevRowKey = abbrevText 10 . _rowKey

mkBoundedText :: KnownNat k => Text -> BoundedText k
mkBoundedText = ensureBound

concatBounded :: Int -> [Text] -> Text
concatBounded bound ts =
  T.concat (go 0 ts)
  where
  go !acc = \case
    x:xs ->
      let acc' = T.length x + acc
      in if acc' > bound then [T.take (bound - acc) x]
      else x : go acc' xs
    [] -> []

renderProvenance :: Provenance -> Text
renderProvenance (Provenance cid mh) =
  -- Note: chainIDs could be user computed
  thsep ["Provenance", abbrevText 10 (_chainId cid), abbrevText 20 (moduleHashToText mh) ]

renderPvAsArgType :: PactValue -> Text
renderPvAsArgType = renderArgTypeError . pactValueToArgTypeError

pactErrorToBoundedText :: PactError info -> BoundedText PactErrorMsgSize
pactErrorToBoundedText = \case
  PELexerError le _ -> lexerErrorToBoundedText le
  PEParseError pe _ -> parseErrorToBoundedText pe
  PEDesugarError de _ -> desugarErrorToBoundedText de
  PEExecutionError ee _ _ -> evalErrorToBoundedText ee
  PEUserRecoverableError ue _ _ -> userRecoverableErrorToBoundedText ue
  PEVerifierError (VerifierError e) _ -> mkBoundedText e

-- | A Pact error code is a 64 bit integer with the following format:
--   0x FF    FF    FF    FF FFFF FFFF
--     |---| |---| |---| |------------|
--       ^     ^     ^          ^ Free bits
--       |     |     |
--       |     |     ----- Inner error cause (e.g No such object in db)
--       |     |
--       |     ------------- Outer error cause (Execution, Name reso, etc)
--       ------------------- Version
-- Note [As of Jul 2 2024]: There are no error versions other than 0,
--      so we don't have a versioning data type yet.
newtype ErrorCode
  = ErrorCode Word64
  deriving (Eq, Ord)

-- | Note: This Show actually defined the error code's serialization.
--   TODO: is this kosher? Or should we expose an `errorCodeToHexString` function?
instance Show ErrorCode where
  show (ErrorCode e) =
    let h = showHex e ""
        len = length h
    in "0x" <> if len < 16 then replicate (16 - len) '0' <> h else h

-- | Our data type for presenting error codes alongside
--   a span info
data PactErrorCode info
  = PactErrorCode
  { _peCode :: ErrorCode
  , _peMsg :: BoundedText PactErrorMsgSize
  , _peInfo :: info
  } deriving (Eq, Show, Functor, Foldable, Traversable)

errorCodeFromText :: Text -> Maybe ErrorCode
errorCodeFromText t = do
  guard (T.length t == 18 && T.all C.isHexDigit (T.drop 2 t))
  case T.hexadecimal t of
    Right (a, remaining) | T.null remaining -> pure $ ErrorCode a
    _ -> Nothing

instance {-# OVERLAPPING #-} J.Encode (PactErrorCode NoInfo) where
  build (PactErrorCode ec msg _) = J.object
    [ "errorCode" J..= T.pack (show ec)
    , "errorMsg" J..= msg
    ]


instance {-# OVERLAPPING #-} JD.FromJSON (PactErrorCode NoInfo) where
  parseJSON = JD.withObject "PactErrorCode" $ \o -> do
    t <- o JD..: "errorCode"
    case errorCodeFromText t of
      Just a -> do
        msg <- o JD..: "errorMsg"
        pure $ PactErrorCode a msg NoInfo
      _ -> fail "failed to parse pact error code"

instance J.Encode info => J.Encode (PactErrorCode info) where
  build (PactErrorCode ec msg info) = J.object
    -- Note: this is safe, the `Show` instance converts it to hex
    [ "errorCode" J..= T.pack (show ec)
    , "errorMsg" J..= msg
    , "info" J..= info ]

instance JD.FromJSON info => JD.FromJSON (PactErrorCode info) where
  parseJSON = JD.withObject "PactErrorCode" $ \o -> do
    t <- o JD..: "errorCode"
    case errorCodeFromText t of
      Just a -> do
        info <- o JD..: "info"
        msg <- o JD..: "errorMsg"
        pure $ PactErrorCode a msg info
      _ -> fail "failed to parse pact error code"

pactErrorToErrorCode :: PactError info -> PactErrorCode info
pactErrorToErrorCode pe = let
  info = view peInfo pe
  -- Inner tag is
  innerTag = shiftL (fromIntegral (innerConstrTag pe)) innerErrorShiftBits
  outerTag = shiftL (fromIntegral (constrIndex pe)) outerErrorShiftBits
  code = ErrorCode (innerTag .|. outerTag)
  in PactErrorCode code (pactErrorToBoundedText pe) info
  where
  innerConstrTag = \case
    PELexerError e _ -> constrIndex e
    PEParseError e _ -> constrIndex e
    PEDesugarError e _ -> constrIndex e
    PEExecutionError e _ _ -> constrIndex e
    PEUserRecoverableError e _ _ -> constrIndex e
    PEVerifierError e _ -> constrIndex e

pactErrorToLocatedErrorCode :: PactError info -> PactErrorCode (LocatedErrorInfo info)
pactErrorToLocatedErrorCode = pactErrorToErrorCode . locatePactErrorInfo

data PrettyErrorCode info
  = PrettyErrorCode
  { _pecFailurePhase :: Text
  , _pecFailureCause :: Text
  , _pecMsg :: Text
  , _pecInfo :: info
  } deriving Show

_versionMask, outerErrorMask, innerErrorMask :: Word64
_versionMask   = 0xFF_00_00_00_00_00_00_00
outerErrorMask = 0x00_FF_00_00_00_00_00_00
innerErrorMask = 0x00_00_FF_00_00_00_00_00

_versionShiftBits, outerErrorShiftBits, innerErrorShiftBits :: Int
_versionShiftBits = 56
outerErrorShiftBits = 48
innerErrorShiftBits = 40

-- | Get the inner and outer cause from an error code
prettyErrorCode :: PactErrorCode info -> PrettyErrorCode info
prettyErrorCode (PactErrorCode (ErrorCode ec) msg i) =
  PrettyErrorCode phase cause (_boundedText msg) i
  where
  getCtorName ctorIx p =
    case find ((== ctorIx) . _ciIndex) (allConstrInfos p) of
      Just c -> _ciName c
      Nothing -> "UNKNOWN_CODE"
  phase =
    let tagIx = (ec .&. outerErrorMask) `shiftR` outerErrorShiftBits
    in getCtorName (fromIntegral tagIx) (Proxy :: Proxy (PactError ()))
  causeTag :: Word8
  causeTag =
    fromIntegral ((ec .&. innerErrorMask) `shiftR` innerErrorShiftBits)
  cause = case phase of
    "PELexerError" -> getCtorName causeTag (Proxy :: Proxy LexerError)
    "PEParseError" -> getCtorName causeTag (Proxy :: Proxy ParseError)
    "PEDesugarError" -> getCtorName causeTag (Proxy :: Proxy DesugarError)
    "PEExecutionError" -> getCtorName causeTag (Proxy :: Proxy EvalError)
    "PEUserRecoverableError" -> getCtorName causeTag (Proxy :: Proxy UserRecoverableError)
    "PEVerifierError" -> getCtorName causeTag (Proxy :: Proxy VerifierError)
    _ -> "UNKNOWN_CODE"

makePrisms ''PactError
makePrisms ''InvariantError
makePrisms ''LexerError
makePrisms ''ParseError
makePrisms ''DesugarError
makePrisms ''EvalError
makePrisms ''UserRecoverableError
makePrisms ''HyperlaneError
makePrisms ''HyperlaneDecodeError
makePrisms ''OwneraError

-- | Legacy error type enums
data LegacyPactErrorType
  = LegacyEvalError
  | LegacyArgsError
  | LegacyDbError
  | LegacyTxFailure
  | LegacySyntaxError
  | LegacyGasError
  | LegacyContinuationError
  deriving (Show,Eq, Bounded, Enum)

instance JD.FromJSON LegacyPactErrorType where
  parseJSON = JD.withText "PactErrorType" $ \case
    "EvalError" -> pure LegacyEvalError
    "ArgsError" -> pure LegacyArgsError
    "DbError" -> pure LegacyDbError
    "TxFailure" -> pure LegacyTxFailure
    "SyntaxError" -> pure LegacySyntaxError
    "GasError" -> pure LegacyGasError
    "ContinuationError" -> pure LegacyContinuationError
    _ -> fail "Invalid legacy pact error type"

instance J.Encode LegacyPactErrorType where
  build LegacyEvalError = J.text "EvalError"
  build LegacyArgsError = J.text "ArgsError"
  build LegacyDbError = J.text "DbError"
  build LegacyTxFailure = J.text "TxFailure"
  build LegacySyntaxError = J.text "SyntaxError"
  build LegacyGasError = J.text "GasError"
  build LegacyContinuationError = J.text "ContinuationError"
  {-# INLINABLE build #-}

-- | LegacyError represents
data LegacyPactError
  = LegacyPactError
  { _leType :: LegacyPactErrorType
  , _leInfo :: Text
  , _leCallStack :: [Text]
  , _leMessage :: Text
  } deriving (Eq, Show)

pactErrorToLegacyErrorType :: PactError i -> LegacyPactErrorType
pactErrorToLegacyErrorType = \case
  PEExecutionError NativeArgumentsError{} _ _ -> LegacyArgsError
  PEExecutionError GasExceeded{} _ _ -> LegacyGasError
  PEExecutionError DbOpFailure{} _ _ -> LegacyDbError
  PEExecutionError ContinuationError{} _ _ -> LegacyContinuationError
  PEUserRecoverableError{} -> LegacyTxFailure
  PEExecutionError{} -> LegacyEvalError
  PEParseError{} -> LegacySyntaxError
  PELexerError{} -> LegacySyntaxError
  PEDesugarError{} -> LegacySyntaxError
  PEVerifierError{} -> LegacyEvalError

-- | Pretty-printed pact error for use in `local` only.
-- Note: DO NOT USE THIS FOR APPLYCMD.
-- I REPEAT: DO NOT USE THIS FOR APPLYCMD.
-- This sort of serialization should _not_ be used to store errors on chain.
-- This pretty prints the error, but it does not care at all about time to serialize
-- For on-chain use, use `pactErrorToErrorCode`
toPrettyLegacyError :: Pretty i => PactError i -> LegacyPactError
toPrettyLegacyError pe =
  let stack = renderText <$> viewErrorStack pe
      info = renderText (view peInfo pe)
  in LegacyPactError (pactErrorToLegacyErrorType pe) info stack (renderText pe)

instance J.Encode LegacyPactError where
  build o = J.object
    [ "callStack" J..= J.Array (_leCallStack o)
    , "type" J..= _leType o
    , "message" J..= _leMessage o
    , "info" J..= _leInfo o
    ]
  {-# INLINE build #-}


instance JD.FromJSON LegacyPactError where
  parseJSON = JD.withObject "LegacyPactError" $ \o -> do
    cs <- o JD..: "callStack"
    ty <- o JD..: "type"
    msg <- o JD..: "message"
    info <- o JD..: "info"
    pure (LegacyPactError ty info cs msg)

-- | PactErrorCompat exists to provide a
--   codec that can understand both pact 4 and pact 5 errors
data PactErrorCompat info
  = PEPact5Error (PactErrorCode info)
  | PELegacyError LegacyPactError
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance J.Encode info => J.Encode (PactErrorCompat info) where
  build = \case
    PEPact5Error err -> J.build err
    PELegacyError err -> J.build err

instance JD.FromJSON info => JD.FromJSON (PactErrorCompat info) where
  parseJSON v =
    (PEPact5Error <$> JD.parseJSON v) <|>
    (PELegacyError <$> JD.parseJSON v)

