{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}

module HW3.Pretty where

import           HW3.Base
import           HW3.Evaluator
import           Prettyprinter 
import           Prettyprinter.Render.Terminal 
import qualified Data.Text as Text
import Data.Ratio
import qualified Data.Sequence as Sequence
import Data.Scientific
import Data.Foldable
import Numeric
import Data.String
import qualified Data.ByteString as ByteString
import Data.Char
import Data.List

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = \case

    HiValueBool True  -> "true"

    HiValueBool False -> "false"

    HiValueNumber num   -> case fromRationalRepetendUnlimited num of
        (n, Nothing) -> case floatingOrInteger @_ @Integer n of
            Right i -> pretty i
            Left f -> pretty $ showFFloat Nothing f ""
        _            -> let n = numerator num
                            d = denominator num
                            (q,r) = quotRem n d
                        in if
            | abs n < abs d -> showDoc n <>  "/"  <> showDoc d
            | n > 0         -> showDoc q <> " + " <> showDoc r       <> "/" <> showDoc (abs d)
            | otherwise     -> showDoc q <> " - " <> showDoc (abs r) <> "/" <> showDoc (abs d)
  
    HiValueFunction f -> case f of
        HiFunDiv            -> "div"       
        HiFunMul            -> "mul"       
        HiFunAdd            -> "add"       
        HiFunSub            -> "sub"   
        HiFunNot            -> "not"      
        HiFunAnd            -> "and"       
        HiFunOr             -> "or"   
        HiFunLessThan       -> "less-than"           
        HiFunGreaterThan    -> "greater-than"               
        HiFunEquals         -> "equals"         
        HiFunNotLessThan    -> "not-less-than"               
        HiFunNotGreaterThan -> "not-greater-than"                  
        HiFunNotEquals      -> "not-equals"             
        HiFunIf             -> "if"     
        HiFunLength         -> "length"         
        HiFunToUpper        -> "to-upper"          
        HiFunToLower        -> "to-lower"          
        HiFunReverse        -> "reverse"          
        HiFunTrim           -> "trim"      
        HiFunList           -> "list"
        HiFunRange          -> "range"
        HiFunFold           -> "fold"
        HiFunPackBytes      -> "pack-bytes"      
        HiFunUnpackBytes    -> "unpack-bytes"       
        HiFunEncodeUtf8     -> "encode-utf8"     
        HiFunDecodeUtf8     -> "decode-utf8"     
        HiFunZip            -> "zip"    
        HiFunUnzip          -> "unzip"  
        HiFunSerialise      -> "serialise"   
        HiFunDeserialise    -> "deserialise"
        HiFunRead           -> "read"  
        HiFunWrite          -> "write"   
        HiFunMkDir          -> "mkdir"   
        HiFunChDir          -> "cd"   
        HiFunParseTime      -> "parse-time"
        HiFunRand           -> "rand"
        HiFunEcho           -> "echo"
        -- HiFunCount          -> "count"  
        -- HiFunKeys           -> "keys" 
        -- HiFunValues         -> "values"     
        -- HiFunInvert         -> "invert"    
            
    HiValueNull       -> "null"
            
    HiValueString t   -> pretty $ show t

    HiValueList l   -> if Sequence.null l
        then "[ ]"
        else "[ " <> (mconcat $ toList $ Sequence.intersperse ", " $ prettyValue <$> l) <> " ]"

    HiValueBytes bs -> renderByteString bs

    HiValueAction a -> case a of
        HiActionRead  fp    -> pretty $ "read(" <> show fp <> ")"
        HiActionWrite fp bs -> pretty ("write(" <> show fp <> ", ") <> renderByteString bs <> ")"
        HiActionMkDir fp    -> pretty $ "mkdir(" <> show fp <> ")"
        HiActionChDir fp    -> pretty $ "cd(" <> show fp <> ")"
        HiActionCwd         -> "cwd"
        HiActionNow         -> "now"
        HiActionRand  a b   -> pretty $ "rand(" <> show a <> ", " <> show b <> ")"
        HiActionEcho  t     -> pretty $ "echo(" <> show t <> ")"
                    
    HiValueTime t   -> "parse-time(" <> pretty (show (show t)) <> ")"
                    
    -- HiValueDict d   -> case M.toList d of
    --     []     -> "{ }"
    --     (x:xs) -> mconcat ["{ ", foldl (\ini y -> ini <> ", " <> renderPair y) (renderPair x) $ xs, " }"]

    x -> error $ show x  

renderByteString :: ByteString.ByteString -> Doc AnsiStyle
renderByteString "" = "[# #]" 
renderByteString bs = pretty $ "[# " <> str <> "#]"
    where 
        str = foldl (\ini b -> ini <> render b <> " ") "" (map fromIntegral $ ByteString.unpack bs)
        render b = if b < 16 then "0" <> showHex b "" else showHex b ""

showDoc :: Show a => a -> Doc AnsiStyle
showDoc = fromString . show