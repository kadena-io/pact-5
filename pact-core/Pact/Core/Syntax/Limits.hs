module Pact.Core.Syntax.Limits where

identifierLengthLimit :: Int
identifierLengthLimit = 64

-- | How many fields is a schema allowed to have
schemaLengthLimit :: Int
schemaLengthLimit = 50

-- | How many fields is a constructed object allowed to have
objectSizeLimit :: Int
objectSizeLimit = 50

-- | Maximum number of function arguments
functionArgLengthLimit :: Int
functionArgLengthLimit = 25

-- | Maximum number of function arguments
rowKeyLengthLimit :: Int
rowKeyLengthLimit = 1024


