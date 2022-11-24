module HW3.Parser
--   ( parse
--   )
   where

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
import HW3.Base -- (HiExpr)
import qualified Text.Megaparsec as Megaparsec
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (symbol, scientific, charLiteral)

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse s = Megaparsec.parse (pExpr <* eof) "" $ cleanSpaces False s

-- Bool here is flag that indicates that we are now parsing string/bytes values where spaces are important.
cleanSpaces :: Bool -> String -> String
cleanSpaces _     []          = []

-- start / end of the string
cleanSpaces b     (x@'\"':xs) = x : cleanSpaces (not b) xs

-- start of the bytes
cleanSpaces _     ('[':'#':xs) = '[' : '#' : cleanSpaces True xs

-- end of the bytes
cleanSpaces _     ('#':']':xs) = '#' : ']' : cleanSpaces False xs

-- cleanSpaces False (' '   :xs) =     cleanSpaces False   xs

cleanSpaces False ('\\' : 'n' :xs) = cleanSpaces False xs
cleanSpaces False (' '   :xs) = cleanSpaces False xs
cleanSpaces False (x     :xs) = x : cleanSpaces False xs
cleanSpaces True  (x     :xs) = x : cleanSpaces True xs

type Parser = Parsec Void String

pExpr :: Parser HiExpr
pExpr = makeExprParser pTerm operatorTable

pTerm :: Parser HiExpr
pTerm = do
    f    <- try (parens pExpr) <|> (HiExprValue <$> pValue) <|> pList
    args <- many $ (flip HiExprApply <$> parens commaItems) <|> (HiExprRun <$ char '!')
    pure $ foldl (&) f args

pList :: Parser HiExpr
pList = fmap (HiExprApply (HiExprValue $ HiValueFunction HiFunList)) $ brackets commaItems

commaItems :: Parser [HiExpr]
commaItems = pExpr `sepBy` char ','

-- Operators

operatorTable :: [[Operator Parser HiExpr]]
operatorTable =
    [   [ binary' InfixL "/"  HiFunDiv
        , binary'  InfixL "*"  HiFunMul
        ]
        ,
        [ binary'  InfixL "-"  HiFunSub
        , binary'  InfixL "+"  HiFunAdd
        ]
        ,
        [ binary'  InfixN "<=" HiFunNotGreaterThan
        , binary'  InfixN ">=" HiFunNotLessThan
        , binary'  InfixN "==" HiFunEquals
        , binary'  InfixN "/=" HiFunNotEquals
        , binary' InfixN "<"  HiFunLessThan
        , binary' InfixN ">"  HiFunGreaterThan 
        ]
        ,
        [ binary'  InfixR "&&" HiFunAnd 
        ]
        ,
        [ binary'  InfixR "||" HiFunOr 
        ]
    ]

-- binary :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -> String -> HiFun -> Operator Parser HiExpr
-- binary assoc name = mkBinary assoc (string name)

binary' :: (Parser (HiExpr -> HiExpr -> HiExpr) -> Operator Parser HiExpr) -> String -> HiFun -> Operator Parser HiExpr
binary' assoc name = mkBinary assoc (try $ (string name) <* notFollowedBy (string "="))

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
    ]

pBool :: Parser HiValue
pBool = fmap HiValueBool $ False <$ string "false" <|> True <$ string "true"

pNum :: Parser HiValue
pNum = HiValueNumber . toRational <$> ((char '-' >> negate <$> scientific) <|> scientific)

pNull :: Parser HiValue
pNull = HiValueNull <$ string "null"

-- Primitives

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

brackets :: Parser a -> Parser a
brackets = between (char '[') (char ']')