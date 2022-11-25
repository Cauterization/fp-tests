module HW3.Parser where

import Control.Monad
import Control.Monad.Combinators.Expr
import qualified Data.Text as Text
import Data.Void (Void)
import Data.String
import Data.Char
import Numeric
import Data.Function
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Sequence as Sequence
import HW3.Base
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (symbol, scientific, charLiteral)
import Data.List (intercalate, intersperse)

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse s = Megaparsec.parse (pExpr <* eof) "" s

-- >>> parse "   div ( add   (    10      ,   15.1    )     ,    3   ) +       2      "
-- Right (HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) [HiExprApply (HiExprValue (HiValueFunction HiFunDiv)) [HiExprApply (HiExprValue (HiValueFunction HiFunAdd)) [HiExprValue (HiValueNumber (10 % 1)),HiExprValue (HiValueNumber (151 % 10))],HiExprValue (HiValueNumber (3 % 1))],HiExprValue (HiValueNumber (2 % 1))])

-- >>> parse add(45  ,  -  15  )  "
-- Left (ParseErrorBundle {bundleErrors = TrivialError 6 (Just (Tokens ('(' :| ""))) (fromList [Tokens ('&' :| "&"),Tokens ('*' :| ""),Tokens ('+' :| ""),Tokens ('-' :| ""),Tokens ('/' :| ""),Tokens ('/' :| "="),Tokens ('<' :| ""),Tokens ('<' :| "="),Tokens ('=' :| "="),Tokens ('>' :| ""),Tokens ('>' :| "="),Tokens ('|' :| "|"),EndOfInput]) :| [], bundlePosState = PosState {pstateInput = "   add(45  ,  -  15  )  ", pstateOffset = 0, pstateSourcePos = SourcePos {sourceName = "", sourceLine = Pos 1, sourceColumn = Pos 1}, pstateTabWidth = Pos 8, pstateLinePrefix = ""}})

-- Bool here is flag that indicates that we are now parsing string/bytes values where spaces are important.
cleanSpaces :: Bool -> String -> String
cleanSpaces _     []          = []

-- start / end of the string
cleanSpaces b     (x@'\"':xs) = x : cleanSpaces (not b) xs

-- start of the bytes
cleanSpaces _     ('[':'#':xs) = '[' : '#' : cleanSpaces True xs

-- end of the bytes
cleanSpaces _     ('#':']':xs) = '#' : ']' : cleanSpaces False xs

-- spaces
cleanSpaces False ('\\':'n':xs) = cleanSpaces False xs
cleanSpaces False (' '     :xs) = cleanSpaces False xs

cleanSpaces False (x     :xs) = x : cleanSpaces False xs
cleanSpaces True  (x     :xs) = x : cleanSpaces True xs

type Parser = Parsec Void String

pExpr :: Parser HiExpr
pExpr = many (satisfy isSpace) *> makeExprParser pTerm operatorTable <* many (satisfy isSpace)

pTerm :: Parser HiExpr
pTerm = do
    many (satisfy isSpace)
    f    <- choice 
        [ parens pExpr
        , HiExprValue <$> pValue
        , pList
        , pDict
        ]
    args <- many $ choice
        [ try $ mSpace $ fmap (flip HiExprApply) $ parens commaItems 
        , fmap (flip HiExprApply) $ char '.' *> (pure . HiExprValue . HiValueString . Text.pack . intercalate "-" <$> (((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'))
        , HiExprRun <$ char '!'
        ]
    many (satisfy isSpace)
    pure $ foldl (&) f args

pList :: Parser HiExpr
pList = fmap (HiExprApply (HiExprValue $ HiValueFunction HiFunList)) $ between (char '[') (char ']') $ mSpace commaItems

pDict :: Parser HiExpr
pDict = fmap HiExprDict $ between (char '{') (char '}') $ pKV `sepBy` char ','
    where
        pKV = liftM2 (,) pExpr $ char ':' *> pExpr

commaItems :: Parser [HiExpr]
commaItems = pExpr `sepBy` char ','

-- Operators

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
    [   [ binary InfixL "/"  HiFunDiv
        , binary InfixL "*"  HiFunMul
        ]
        ,
        [ binary InfixL "-"  HiFunSub
        , binary InfixL "+"  HiFunAdd
        ]
        ,
        [ binary InfixN "<=" HiFunNotGreaterThan
        , binary InfixN ">=" HiFunNotLessThan
        , binary InfixN "==" HiFunEquals
        , binary InfixN "/=" HiFunNotEquals
        , binary InfixN "<"  HiFunLessThan
        , binary InfixN ">"  HiFunGreaterThan 
        ]
        ,
        [ binary InfixR "&&" HiFunAnd 
        ]
        ,
        [ binary InfixR "||" HiFunOr 
        ]
    ]

binary :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -> String -> HiFun -> Operator Parser HiExpr
binary assoc name = mkBinary assoc (try $ (string name) <* notFollowedBy (string "="))

mkBinary :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -> Parser a -> HiFun -> Operator Parser HiExpr
mkBinary assoc p f = assoc ((\a b -> HiExprApply (HiExprValue $ HiValueFunction f) [a, b]) <$ p)

-- Values

pValue :: Parser HiValue
pValue = choice
    [ parens pValue
    , pNum
    , pFun
    , pBool
    , pNull
    , pString
    , pBytes
    , pAction
    ]

pAction :: Parser HiValue
pAction = fmap HiValueAction $ (HiActionCwd <$ string "cwd") <|> (HiActionNow <$ string "now")

pBytes :: Parser HiValue
pBytes = fmap HiValueBytes $ (<|> emptyBytes) $ try $ between (string "[#") (string "#]") $ do
    char ' ' <|> pure ' '
    bHead <- pHex
    bTail <- many $ try $ char ' ' *> pHex
    char ' ' <|> pure ' '
    return $ fromString $ bHead : bTail
  where
    emptyBytes = between (string "[#") (string "#]") $ do
        many $ char ' '
        pure $ ByteString.pack ""
    pHex = do
        d1 <- satisfy isHexDigit
        d2 <- satisfy isHexDigit
        let res = readHex [d1, d2]
        guard $ not $ null $ res
        return $ chr $ fst $ head $ res

pString :: Parser HiValue
pString = fmap (HiValueString . Text.pack) $ char '"' >> manyTill charLiteral (char '"')

pFun :: Parser HiValue
pFun = HiValueFunction <$> choice 

    -- arithmetic
    [ HiFunDiv <$ string "div"
    , HiFunMul <$ string "mul"
    , HiFunSub <$ string "sub"
    , HiFunAdd <$ string "add"

    -- comparison
    , HiFunNotLessThan    <$ string "not-less-than"
    , HiFunNotGreaterThan <$ string "not-greater-than"
    , HiFunNotEquals      <$ string "not-equals"
    , HiFunIf             <$ string "if"
    , HiFunNot            <$ string "not" -- <* notFollowedBy "-")
    , HiFunAnd            <$ string "and"
    , HiFunOr             <$ string "or"
    , HiFunLessThan       <$ string "less-than"
    , HiFunGreaterThan    <$ string "greater-than"
    , HiFunEquals         <$ string "equals"

    -- string and slices
    , HiFunLength        <$ string "length"
    , HiFunToUpper       <$ string "to-upper" 
    , HiFunToLower       <$ string "to-lower" 
    , HiFunReverse       <$ string "reverse" 
    , HiFunTrim          <$ string "trim" 

    -- lists and folds
    , HiFunList          <$ string "list"
    , HiFunRange         <$ string "range" 
    , HiFunFold          <$ string "fold"

    -- bytes
    , HiFunPackBytes     <$ string "pack-bytes"
    , HiFunUnpackBytes   <$ string "unpack-bytes"  
    , HiFunZip           <$ string "zip"
    , HiFunUnzip         <$ string "unzip" 
    , HiFunEncodeUtf8    <$ string "encode-utf8"   
    , HiFunDecodeUtf8    <$ string "decode-utf8"  
    , HiFunSerialise     <$ string "serialise" 
    , HiFunDeserialise   <$ string "deserialise"  

    -- actions
    , HiFunRead          <$ string "read"
    , HiFunWrite         <$ string "write"
    , HiFunMkDir         <$ string "mkdir"
    , HiFunChDir         <$ string "cd"

    -- time
    , HiFunParseTime     <$ string "parse-time"

    --rand
    , HiFunRand          <$ string "rand"

    -- echo
    , HiFunEcho          <$ string "echo"

    -- dicts
    , HiFunCount         <$ string "count"
    , HiFunKeys          <$ string "keys"
    , HiFunValues        <$ string "values"
    , HiFunInvert        <$ string "invert"
    ]

pBool :: Parser HiValue
pBool = fmap HiValueBool $ False <$ string "false" <|> True <$ string "true"

pNum :: Parser HiValue
pNum = HiValueNumber . toRational <$> ((char '-' >> negate <$> mSpace scientific) <|> scientific)

pNull :: Parser HiValue
pNull = HiValueNull <$ string "null"

-- Primitives

parens :: Parser a -> Parser a
parens p = between (char '(') (mSpace $ char ')') $ mSpace p

mSpace :: Parser a -> Parser a
mSpace = ((many $ satisfy isSpace) *>)
