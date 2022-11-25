{-# LANGUAGE LambdaCase, OverloadedStrings, MultiWayIf, TypeApplications #-}

module HW3.Pretty where

import HW3.Base
    ( HiAction(..),
      HiFun(..),
      HiValue(..) )
import Prettyprinter ( Doc, Pretty(pretty) ) 
import Prettyprinter.Render.Terminal 
    ( AnsiStyle
    ) 
import qualified Data.Text as Text
import Data.Ratio ( denominator, numerator )
import qualified Data.Sequence as Sequence
import Data.Scientific
    ( floatingOrInteger, fromRationalRepetendUnlimited )
import Data.Foldable ( Foldable(toList) )
import Numeric ( showFFloat, showHex )
import Data.String ( IsString(fromString) )
import qualified Data.ByteString as ByteString
import qualified Data.Map as Map

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue = \case

    HiValueBool True  -> "true"

    HiValueBool False -> "false"

    HiValueNumber num   -> pretty $ case fromRationalRepetendUnlimited num of
        (n, Nothing) -> case floatingOrInteger @_ @Integer n of
            Right i -> show i
            Left f -> showFFloat Nothing f ""
        _            -> let n = numerator num
                            d = denominator num
                            (q,r) = quotRem n d
                        in if
            | abs n < abs d -> show n <>  "/"  <> show d
            | n > 0         -> show q <> " + " <> show r       <> "/" <> show (abs d)
            | otherwise     -> show q <> " - " <> show (abs r) <> "/" <> show (abs d)
  
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
        HiFunCount          -> "count"  
        HiFunKeys           -> "keys" 
        HiFunValues         -> "values"     
        HiFunInvert         -> "invert"    
            
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
                    
    HiValueTime t   -> pretty $ "parse-time(" <> (show (show t)) <> ")"
                    
    HiValueDict d   -> case Map.toList d of
        []     -> "{ }"
        (x:xs) -> let render (a,b) = prettyValue a <> ": " <> prettyValue b
                  in "{ " <> foldl (\ini y -> ini <> ", " <> render y) (render x) xs <> " }"

renderByteString :: ByteString.ByteString -> Doc AnsiStyle
renderByteString "" = "[# #]" 
renderByteString bs = pretty $ "[# " <> str <> "#]"
    where 
        str = foldl (\ini b -> ini <> render b <> " ") "" (map fromIntegral $ ByteString.unpack bs)
        render b = if b < 16 then "0" <> showHex b "" else showHex b ""

showDoc :: Show a => a -> Doc AnsiStyle
showDoc = fromString . show