{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
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
 , DbOpException(..)
 , HyperlaneError(..)
 , HyperlaneDecodeError(..)
 , peInfo
 , viewErrorStack
 , UserRecoverableError(..)
 ) where

import Control.Lens hiding (ix)
import Control.Exception
import Data.Text(Text)
import Data.Dynamic (Typeable)
import Data.Set(Set)
import qualified Data.Version as V
import qualified PackageInfo_pact_tng as PI
import qualified Data.Set as S
import qualified Data.Text as T

import Control.DeepSeq
import GHC.Generics

import Pact.Core.Type
import Pact.Core.Names
import Pact.Core.Guards
import Pact.Core.Info
import Pact.Core.Gas.Types
import Pact.Core.Pretty as Pretty
import Pact.Core.Hash
import Pact.Core.StackFrame
import Pact.Core.DefPacts.Types
import Pact.Core.PactValue
import Pact.Core.Capabilities
import Pact.Core.Verifiers

type PactErrorI = PactError SpanInfo

data LexerError
  = LexicalError Char Char
  -- ^ Lexical error: encountered character, last seen character
  | InvalidIndentation Int Int
  -- ^ Invalid indentation: ^ current indentation, expected indentation
  | InvalidInitialIndent Int
  -- ^ Invalid initial indentation: got ^, expected 2 or 4
  | StringLiteralError Text
  -- ^ Error lexing string literal
  | OutOfInputError Char
  deriving (Show, Generic)

instance NFData LexerError

instance Exception LexerError

instance Pretty LexerError where
  pretty = ("Lexical Error: " <>) . \case
    LexicalError c1 c2 ->
      Pretty.hsep ["Encountered character",  Pretty.parens (pretty c1) <> ",", "Last seen", Pretty.parens (pretty c2)]
    InvalidIndentation curr expected ->
      Pretty.hsep ["Invalid indentation. Encountered", Pretty.parens (pretty curr) <> ",", "Expected", Pretty.parens (pretty expected)]
    StringLiteralError te ->
      Pretty.hsep ["String literal parsing error: ", pretty te]
    InvalidInitialIndent i ->
      Pretty.hsep ["Invalid initial ident. Valid indentation are 2 or 4 spaces. Found: ", Pretty.parens (pretty i)]
    OutOfInputError c ->
      Pretty.hsep ["Ran out of input before finding a lexeme. Last Character seen: ", Pretty.parens (pretty c)]

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
  deriving (Show, Generic)

instance NFData ParseError

instance Exception ParseError

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
  | EmptyDefPact Text
  -- ^ Defpact without steps
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
  deriving (Show,  Generic)

instance NFData DesugarError

instance Exception DesugarError

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
      Pretty.hsep ["Invalid governance:", pretty gov]
    InvalidDefInTermVariable n ->
      Pretty.hsep ["Invalid definition in term variable position:", pretty n]
    InvalidModuleReference mn ->
      Pretty.hsep ["Invalid Interface attempted to be used as module reference:", pretty mn]
    EmptyBindingBody -> "Bind expression lacks an accompanying body"
    EmptyDefPact dp -> Pretty.hsep ["Defpact has no steps:", pretty dp]
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
  deriving (Show,  Generic)

instance NFData ArgTypeError

instance Pretty ArgTypeError where
  pretty = \case
    ATEPrim p -> Pretty.brackets $ pretty p
    ATEList -> "list"
    ATEObject -> "object"
    ATETable -> "table"
    ATEClosure -> "closure"
    ATEModRef -> "modref"

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


-- | All fatal execution errors which should pause
--
data EvalError
  = ArrayOutOfBoundsException Int Int
  -- ^ Array index out of bounds <length> <index>
  | ArithmeticException Text
  -- ^ Arithmetic error <cause>
  | EnumerationError Text
  -- ^ Enumeration error (e.g incorrect bounds with step
  | DecodeError Text
  -- ^ Some form of decoding error
  | GasExceeded MilliGasLimit MilliGas
  -- ^ Gas went past the gas limit
  | FloatingPointError Text
  -- ^ Floating point operation exception
  | CapNotInScope Text
  -- ^ Capability not in scope
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
  -- ^ Crosschain DefPact contunation must be at least 2 steps before CC continuation step
  --   with <ccExec> <dbExec>
  | NoPreviousDefPactExecutionFound DefPactStep
  -- ^ No previouse DefPact execution could be found in the environment or database
  | DefPactAlreadyCompleted DefPactStep
  -- ^ DefPact already completed
  | NestedDefPactParentStepCountMismatch DefPactId Int Int
  -- ^ Nested DefPact <stepcount> does not match <parent step count>
  | NestedDefPactParentRollbackMismatch DefPactId Bool Bool
  -- ^ Nested DefPact <rollback> does not match <parent rollback>
  | NestedDefPactNeverStarted DefPactStep
  -- ^ Nested DefPact never started at prior step
  | NestedDefPactDoubleExecution DefPactStep
  -- ^ Nested DefPact is executed twice
  | MultipleOrNestedDefPactExecFound DefPactExec
  -- ^ Unexpected DefPactExec found in the environment
  | DefPactStepHasNoRollback DefPactStep
  -- ^ The requested DefPactStep has no rollback
  | DefPactStepNotInEnvironment
  -- ^ DefPactStep is not in the environment
  | NoDefPactIdAndExecEnvSupplied
  -- ^ No DefPactId supplied and no DefPactExec found in the environment
  | DefPactRollbackMismatch DefPactStep DefPactExec
  -- ^ DefPact rollback missmatch
  | DefPactStepMismatch DefPactStep DefPactExec
  -- ^ DefPact missmatch
  | CannotUpgradeInterface ModuleName
  -- ^ Interface cannot be upgrade
  | DbOpFailure DbOpException
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
  | NamespaceInstallError Text
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
  | ModRefImplementsNoInterfaces ModuleName
  -- ^ Attempted to use a module as a modref, despite
  -- implementing no interfaces
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
  | HyperlaneError HyperlaneError
  | HyperlaneDecodeError HyperlaneDecodeError
  deriving (Show, Generic)

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
    -- Todo: probably enhance this data type
    CapNotInScope txt ->
      Pretty.hsep ["Capability not in scope:", pretty txt]
    GasExceeded (MilliGasLimit (milliGasToGas -> Gas limit)) (milliGasToGas -> Gas amt) ->
      "Gas Limit:" <+> parens (pretty limit) <+> "exceeded:" <+> pretty amt
    InvariantFailure msg ->
      Pretty.hsep ["Fatal execution error, invariant violated:", pretty msg]
    NativeArgumentsError n tys ->
      Pretty.hsep ["Native evaluation error for native", pretty n <> ",", "received incorrect argument(s) of type(s)", Pretty.commaSep tys]
    EvalError txt ->
      Pretty.hsep ["Program encountered an unhandled raised error:", pretty txt]
    YieldOutsideDefPact ->
      "Try to yield a value outside a running DefPact execution"
    NoActiveDefPactExec ->
      "No active DefPact execution in the environment"
    NoYieldInDefPactStep (DefPactStep step _ i _) ->
      Pretty.hsep ["No yield in DefPactStep:", "Step: " <> pretty step, "DefPactId: " <> pretty i]
    InvalidDefPactStepSupplied (DefPactStep step _ _ _) stepCount ->
      Pretty.hsep
      [ "DefPactStep does not match DefPact properties:"
      , "requested: "<> pretty step
      , "step count:" <> pretty stepCount]
    DefPactIdMismatch reqId envId ->
      Pretty.hsep
      [ "Requested DefPactId:", pretty reqId
      , "does not match context DefPactId:", pretty envId
      ]
    CCDefPactContinuationError pactStep _ccExec _dbExec ->
      Pretty.hsep
      [ "Crosschain DefPact continuation error:"
      , "DefPactId:" <> pretty (_psStep pactStep)
      ]
    NestedDefPactParentRollbackMismatch pid rollback parentRollback ->
      Pretty.hsep
      [ "Nested DefPact execution failed, parameter missmatch:"
      , "DefPactId: " <> pretty pid
      , "Rollback: " <> pretty rollback
      , "Parent rollback:" <> pretty parentRollback
      ]
    NestedDefPactParentStepCountMismatch pid stepCount parentStepCount ->
      Pretty.hsep
      [ "Nested DefPact execution failed, parameter missmatch:"
      , "PacId: " <> pretty pid
      , "step count: " <> pretty stepCount
      , "Parent step count: " <> pretty parentStepCount
      ]
    NoPreviousDefPactExecutionFound ps ->
      Pretty.hsep ["No previous DefPact exeuction found for DefPactId: ", pretty (_psDefPactId ps)]
    DefPactAlreadyCompleted ps -> Pretty.hsep
      [ "Requested DefPact already completed: ", "DefPactId:" <> pretty (_psDefPactId ps)]
    NestedDefPactNeverStarted ps -> Pretty.hsep
      ["Requested nested DefPact never started:", "DefPactId: " <> pretty (_psDefPactId ps)]
    NestedDefPactDoubleExecution ps -> Pretty.hsep
      ["Requested nested DefPact double execution:", "DefPactId: " <> pretty (_psDefPactId ps)]
    MultipleOrNestedDefPactExecFound pe -> Pretty.hsep
      ["DefPact execution context already in the environment: ", "DefPactId: " <> pretty (_peDefPactId pe)]
    DefPactStepHasNoRollback ps -> Pretty.hsep
      ["Step has no rollback:", "DefPactId: " <> pretty (_psDefPactId ps)]
    DefPactStepNotInEnvironment -> "No DefPactStep in the environment"
    NoDefPactIdAndExecEnvSupplied -> "No DefPactId or execution environment supplied"
    DefPactRollbackMismatch ps pe -> Pretty.hsep
      [ "Rollback missmatch in DefPactStep and DefPact exeuction environment:"
      , "DefPactId: " <> pretty (_psDefPactId ps)
      , "step rollback: " <> pretty (_psRollback ps)
      , "DefPactExec rollback: " <> pretty (_peStepHasRollback pe)
      ]
    DefPactStepMismatch ps pe -> Pretty.hsep
      [ "Step missmatch in DefPactStep and DefPact exeuction environment:"
      , "DefPactId: " <> pretty (_psDefPactId ps)
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
      "Nested defpacts not advanced" <+> pretty dpid
    ExpectedPactValue ->
      "Expected Pact Value, got closure or table reference"
    NotInDefPactExecution -> "not in pact execution"
    NamespaceInstallError e ->
      "Namespace installation error:" <+> pretty e
    PointNotOnCurve ->
      "Point lies outside of ellptic curve"
    YieldProvenanceDoesNotMatch received expected ->
      "Yield provenance does not match, received" <+> pretty received <> ", expected" <+> pretty expected
    MismatchingKeysetNamespace ns ->
      "Error defining keyset, namespace mismatch, expected " <> pretty ns
    RuntimeRecursionDetected qn ->
      "Runtime recursion detected in function:" <+> pretty qn
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
      "Module not found:" <+> pretty nsn
    ModRefImplementsNoInterfaces mn ->
      "Invalid modref, module" <+> pretty mn <+> "implements no interfaces"
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

instance Exception EvalError

data DbOpException
  = WriteException
  | RowReadDecodeFailure Text
  | RowFoundException TableName RowKey
  | NoRowFound TableName RowKey
  | NoSuchTable TableName
  | TableAlreadyExists TableName
  | TxAlreadyBegun Text
  | NoTxToCommit
  | OpDisallowed
  | MultipleRowsReturnedFromSingleWrite
  deriving (Show, Eq, Typeable, Generic)

instance NFData DbOpException

instance Exception DbOpException

instance Pretty DbOpException where
  pretty = \case
    WriteException ->
      "Error found while writing value"
    RowReadDecodeFailure rk ->
      "Failed to deserialize but found value at key:" <> pretty rk
    RowFoundException tn rk ->
      "Value already found while in Insert mode in table" <+> pretty tn <+> "at key" <+> dquotes (pretty rk)
    NoRowFound tn rk ->
      "No row found during update in table" <+> pretty tn <+> "at key" <+> pretty rk
    NoSuchTable tn ->
      "Table" <+> pretty tn <+> "not found"
    TableAlreadyExists tn ->
      "Table" <+> pretty tn <+> "already exists"
    TxAlreadyBegun tx ->
      "Attempted to begin tx" <+> dquotes (pretty tx) <> ", but a tx already has been initiated"
    NoTxToCommit ->
      "No Transaction to commit"
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
instance Exception UserRecoverableError

instance Pretty UserRecoverableError where
  pretty = \case
    UserEnforceError t -> pretty t
    OneShotCapAlreadyUsed -> "Automanaged capability used more than once"
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
    deriving (Show, Generic)

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
  deriving (Show, Generic)

instance NFData HyperlaneDecodeError

instance Pretty HyperlaneDecodeError where
  pretty = \case
    HyperlaneDecodeErrorBase64 -> "Failed to base64-decode token message"
    HyperlaneDecodeErrorInternal errmsg -> "Decoding error: " <> pretty errmsg
    HyperlaneDecodeErrorBinary -> "Decoding error: binary decoding failed"
    HyperlaneDecodeErrorParseRecipient -> "Could not parse recipient into a guard"

data PactError info
  = PELexerError LexerError info
  | PEParseError ParseError info
  | PEDesugarError DesugarError info
  -- | PETypecheckError TypecheckError info
  -- | PEOverloadError OverloadError info
  | PEExecutionError EvalError [StackFrame info] info
  | PEUserRecoverableError UserRecoverableError [StackFrame info] info
  deriving (Show, Functor, Generic)

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

viewErrorStack :: PactError info -> [StackFrame info]
viewErrorStack = \case
  PEExecutionError _ stack _ -> stack
  PEUserRecoverableError _ stack _ -> stack
  _ -> []

instance (Show info, Typeable info) => Exception (PactError info)
