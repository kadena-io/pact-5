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
functionSizeLimit :: Int
functionSizeLimit = 25

-- | Maximum number of function arguments
rowKeyMaxSizeLimit :: Int
rowKeyMaxSizeLimit = 1024


