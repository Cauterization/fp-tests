{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module HW3.Base where

import qualified Codec.Serialise as Serialise
import Data.ByteString ( ByteString )
import Data.Text ( Text )
import qualified Data.Time as Time
import Data.Sequence ( Seq ) 
import GHC.Generics ( Generic )
import Data.Map ( Map ) 
data HiFun -- function names (e.g. div, sort, length, ...)

  = HiFunDiv --p7 l
  | HiFunMul --p7 l
  | HiFunAdd --p6 l
  | HiFunSub --p6 l

  | HiFunNot
  | HiFunAnd --p3 r
  | HiFunOr --p2 r
  | HiFunLessThan --p4
  | HiFunGreaterThan --p4
  | HiFunEquals --p4
  | HiFunNotLessThan --p4
  | HiFunNotGreaterThan --p4
  | HiFunNotEquals --p4
  | HiFunIf

  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim

  | HiFunList
  | HiFunRange
  | HiFunFold

  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise

  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir

  | HiFunParseTime

  | HiFunRand

  | HiFunEcho

  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert

  deriving (Show, Eq, Ord, Generic, Serialise.Serialise)

data HiValue -- values (numbers, booleans, strings, ...)
  = HiValueNumber Rational
  | HiValueFunction HiFun

  | HiValueBool Bool

  | HiValueNull
  | HiValueString Text

  | HiValueList (Seq HiValue)

  | HiValueBytes ByteString

  | HiValueAction HiAction

  | HiValueTime Time.UTCTime

  | HiValueDict (Map HiValue HiValue)

  deriving (Show, Eq, Ord, Generic, Serialise.Serialise)

data HiExpr -- expressions (literals, function calls, ...)
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving (Show, Eq, Ord, Generic, Serialise.Serialise)

data HiError -- evaluation errors (invalid arguments, ...)
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq, Ord)

data HiAction = -- actions
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd

  | HiActionNow
  
  | HiActionRand Int Int

  | HiActionEcho Text
  deriving (Show, Eq, Ord, Generic, Serialise.Serialise)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue
