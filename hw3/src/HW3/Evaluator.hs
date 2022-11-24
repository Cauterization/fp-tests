{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeApplications #-}

module HW3.Evaluator where

import Data.Semigroup
import Data.Char
import Numeric
import Data.Foldable
import Data.String
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as C8ByteString
import qualified Data.ByteString.Lazy as LByteString
import HW3.Base
import Control.Monad.Except
import qualified Codec.Compression.Zlib as Zlib
import qualified Codec.Serialise as Serialise
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import HW3.Utils
import qualified Data.Sequence as Sequence
import Text.Read (readMaybe)
import qualified Data.Time as Time

type EvalT m a = ExceptT HiError m a

instance HiMonad m => HiMonad (ExceptT HiError m) where
    runAction = lift . runAction

pattern FApp f args = HiExprApply (FExp f) args
pattern LApp l args = HiExprApply (LExp l) args
pattern BApp b args = HiExprApply (BExp b) args

pattern FExp f = HiExprValue (FVal f)
pattern BExp b = HiExprValue (BVal b)
pattern NExp n = HiExprValue (NVal n)
pattern SExp s = HiExprValue (SVal s)
pattern LExp l = HiExprValue (LValA l)

pattern FVal  f = HiValueFunction f
pattern NVal  n = HiValueNumber n
pattern SVal  s = HiValueString s
pattern BVal  b = HiValueBytes b
pattern LVal0            = HiValueList Sequence.Empty
pattern LVal1 v1         = HiValueList (v1 Sequence.:<| Sequence.Empty)
pattern LValN l         <- HiValueList l@(v1 Sequence.:<| (v2 Sequence.:<| rest))
pattern LValA l          = HiValueList l

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalT

evalT :: HiMonad m => HiExpr -> EvalT m HiValue
evalT = \case

    -- value

    HiExprValue val -> return val

    -- run

    HiExprRun exp -> evalT exp >>= \case
        -- HiValueAction a@(HiActionRand l u) -> if
        --         | l == u -> return $ HiValueNumber $ fromIntegral l
        --         | max (abs l) (abs u) > 1234567898765432122 -> throwError $ HiErrorInvalidArgument
        --         | otherwise -> runAction a
        -- HiValueAction a@(HiActionRand l r) -> if
        --     | any (> (maxBound @Int)) [l, r] -> undefined
        HiValueAction a -> runAction a 
        _               -> throwError HiErrorInvalidArgument

    -- arithmetic
    FApp HiFunDiv       [l, r] -> (,) <$> evalT l <*> evalT r >>= \case 
        (NVal l, NVal 0) -> throwError HiErrorDivideByZero
        (NVal l, NVal r) -> return $ HiValueNumber $ l / r
        (SVal l, SVal r) -> return $ HiValueString $ l <> "/" <> r
        _                -> throwError HiErrorInvalidArgument

    FApp HiFunMul       [l, r] -> (,) <$> evalT l <*> evalT r >>= \case 
        (NVal l, NVal r) -> return $ HiValueNumber $ l * r
        (SVal s, NVal n) -> case ratToInt n of
            Just i -> if i <= 0 
                      then throwError HiErrorInvalidArgument 
                      else return $ HiValueString $ stimes i s
            _      -> throwError HiErrorInvalidArgument 
        (HiValueList l, NVal n) -> case ratToInt n of
            Just i -> if i <= 0 
                      then throwError HiErrorInvalidArgument 
                      else return $ HiValueList $ stimes i l
            _      -> throwError HiErrorInvalidArgument 
        (HiValueBytes b, NVal n) -> case ratToInt n of
            Just i -> if i <= 0 
                      then throwError HiErrorInvalidArgument 
                      else return $ HiValueBytes $ stimes i b
            _      -> throwError HiErrorInvalidArgument 
        _                -> throwError HiErrorInvalidArgument

    FApp HiFunSub       [l, r] -> (,) <$> evalT l <*> evalT r >>= \case 
        (NVal l, NVal r) -> return $ HiValueNumber $ l - r
        (HiValueTime   l, HiValueTime r) -> return $ HiValueNumber $ toRational $ Time.diffUTCTime l r
        _                -> throwError HiErrorInvalidArgument

    FApp HiFunAdd      [l, r] -> (,) <$> evalT l <*> evalT r >>= \case 

        (NVal l, NVal r) -> return $ HiValueNumber $ l + r
        (SVal l, SVal r) -> return $ HiValueString $ l <> r
        (HiValueList l, HiValueList r) -> return $ HiValueList $ l <> r
        (HiValueBytes l, HiValueBytes r) -> return $ HiValueBytes $ l <> r
        (HiValueTime  t, HiValueNumber n) -> return $ HiValueTime $ Time.addUTCTime (realToFrac n) t
        _                -> throwError HiErrorInvalidArgument

    -- booleans
    FApp HiFunNot       [x]    -> evalT x >>= \case
        HiValueBool b -> pure $ HiValueBool $ not b
        _             -> throwError HiErrorInvalidArgument

    FApp HiFunAnd       [l, r] -> do
        le <- evalT l
        case le of
            HiValueBool False -> return $ HiValueBool False
            HiValueNull       -> return $ HiValueNull
            _                 -> evalT r

    FApp HiFunOr       [l, r] -> do
        le <- evalT l
        case le of
            HiValueBool False -> evalT r
            HiValueNull       -> evalT r
            _                 -> return le

    FApp HiFunEquals    [l, r] -> HiValueBool <$> evalEq l r

    FApp HiFunNotEquals [l, r] -> HiValueBool . not <$> evalEq l r

    FApp HiFunLessThan  [l, r] -> HiValueBool <$> evalLt l r

    FApp HiFunGreaterThan [l, r] -> HiValueBool . not <$> ((||) <$> evalEq l r <*> evalLt l r)

    FApp HiFunNotLessThan [l, r] -> HiValueBool . not <$> evalLt l r

    FApp HiFunNotGreaterThan [l, r] -> HiValueBool <$> ((||) <$> evalEq l r <*> evalLt l r)

    FApp HiFunIf [exp, l, r] -> evalT exp >>= \case
            HiValueBool True  -> evalT l
            HiValueBool False -> evalT r
            HiValueNull       -> evalT r
            _                 -> throwError HiErrorInvalidArgument
        
    -- strings and slices

    FApp HiFunLength [exp] -> evalT exp >>= \case
        HiValueString str -> return $ HiValueNumber $ fromIntegral $ Text.length     str
        HiValueList   lst -> return $ HiValueNumber $ fromIntegral $ Sequence.length lst
        HiValueBytes  bs -> return $ HiValueNumber $ fromIntegral $ ByteString.length bs
        _                              -> throwError HiErrorInvalidArgument

    FApp HiFunToUpper [exp] -> evalT exp >>= \case
        HiValueString str -> return $ HiValueString $ Text.toUpper str
        _                              -> throwError HiErrorInvalidArgument

    FApp HiFunToLower [exp] -> evalT exp >>= \case
        HiValueString str -> return $ HiValueString $ Text.toLower str
        _                              -> throwError HiErrorInvalidArgument

    FApp HiFunReverse [exp] -> evalT exp >>= \case
        HiValueString str -> return $ HiValueString $ Text.reverse     str
        HiValueList   lst -> return $ HiValueList   $ Sequence.reverse lst
        HiValueBytes  bs  -> return $ HiValueBytes  $ ByteString.reverse bs
        _                              -> throwError HiErrorInvalidArgument

    FApp HiFunTrim [exp] -> evalT exp >>= \case
        HiValueString str -> return $ HiValueString $ Text.strip str
        _                              -> throwError HiErrorInvalidArgument

    HiExprApply (SExp s) [exp] -> evalT exp >>= \case
        NVal n -> do
            i <- fromIntegral <$> readHiInt n
            return $ if i >= Text.length s || i < 0
            then HiValueNull
            else HiValueString $ Text.pack $ (:[]) $ Text.index s i
        _ -> throwError HiErrorInvalidArgument

    HiExprApply (SExp s) args -> evalContainer s args

    -- lists and folds

    FApp HiFunList args -> HiValueList . Sequence.fromList <$> traverse evalT args

    FApp HiFunRange [l, r] -> (,) <$> evalT l <*> evalT r >>= \case 
        (NVal l, NVal r) -> return . HiValueList . Sequence.fromList $ map HiValueNumber [l .. r]
        _                -> throwError HiErrorInvalidArgument

    FApp HiFunFold args -> traverse evalT args >>= \case
        [_     , LVal0                    ] -> return HiValueNull
        [_     , LVal1 v                  ] -> return v
        [f, LValN (v Sequence.:<| vs)] -> foldM (\a b -> evalT $ HiExprApply (HiExprValue f) (map HiExprValue [a, b])) v vs
        -- [FVal f, LValN (v Sequence.:<| vs)] -> foldM (\a b -> evalT $ HiExprApply (HiExprValue (HiValueFunction f)) (map HiExprValue [a, b])) v vs
        -- [_     , HiValueList _            ] -> throwError HiErrorInvalidFunction
        _ -> throwError HiErrorInvalidArgument

    LApp l args -> evalContainer l args

    -- bytes

    FApp HiFunPackBytes args -> traverse evalT args >>= \case
        [LValA l] -> do
            let validateHex (HiValueNumber r) = do
                    n <- readHiInt r
                    if n >= 0 && n < 256 then return n else throwError HiErrorInvalidArgument 
                validateHex _ = throwError HiErrorInvalidArgument 

            ints <- traverse validateHex l
            return $ HiValueBytes $ ByteString.pack $ fromIntegral <$> toList ints
        _       -> throwError HiErrorInvalidArgument 
        
    FApp HiFunUnpackBytes args -> traverse evalT args >>= \case
        [BVal b] -> return $ HiValueList $ Sequence.fromList $ map (HiValueNumber . fromIntegral . ord) $ C8ByteString.unpack b
        _          -> throwError HiErrorInvalidArgument

    FApp HiFunEncodeUtf8 args -> traverse evalT args >>= \case
        [SVal s] -> return $ HiValueBytes $ Text.encodeUtf8 s
        _        -> throwError HiErrorInvalidArgument

    FApp HiFunDecodeUtf8 args -> traverse evalT args >>= \case
        [BVal b] -> return $ case Text.decodeUtf8' b of
            Right s -> HiValueString s
            _       -> HiValueNull
        _          -> throwError HiErrorInvalidArgument

    FApp HiFunZip args -> traverse evalT args >>= \case
        [BVal b] -> return $ HiValueBytes $ compress b
        _         -> throwError HiErrorInvalidArgument

    FApp HiFunUnzip args -> traverse evalT args >>= \case
        [BVal b] -> return $ HiValueBytes $ LByteString.toStrict $ Zlib.decompress $ LByteString.fromStrict b
        _         -> throwError HiErrorInvalidArgument

    FApp HiFunSerialise args -> traverse evalT args >>= \case
        [x] -> return $ HiValueBytes $ LByteString.toStrict $ Serialise.serialise x
        _         -> throwError HiErrorInvalidArgument

    FApp HiFunDeserialise args -> traverse evalT args >>= \case
        [BVal b] -> return $ case Serialise.deserialiseOrFail $ LByteString.fromStrict b of
            Left _  -> HiValueNull
            Right v -> v
        _         -> throwError HiErrorInvalidArgument

    BApp b args -> evalContainer b args

    -- actions 

    FApp HiFunRead args -> traverse evalT args >>= \case
        [SVal fp] -> return $ HiValueAction $ HiActionRead (Text.unpack fp)
        _                 -> throwError HiErrorInvalidArgument

    FApp HiFunWrite args -> traverse evalT args >>= \case
        [SVal fp, BVal b] -> return $ HiValueAction $ HiActionWrite (Text.unpack fp) b
        _                 -> throwError HiErrorInvalidArgument

    FApp HiFunMkDir args -> traverse evalT args >>= \case
        [SVal fp] -> return $ HiValueAction $ HiActionMkDir (Text.unpack fp)
        _                 -> throwError HiErrorInvalidArgument

    FApp HiFunChDir args -> traverse evalT args >>= \case
        [SVal fp] -> return $ HiValueAction $ HiActionChDir (Text.unpack fp)
        _                 -> throwError HiErrorInvalidArgument

    -- time

    FApp HiFunParseTime args -> traverse evalT args >>= \case
        [HiValueString s] -> return $ maybe HiValueNull HiValueTime (readMaybe $ Text.unpack s)
        _           -> throwError HiErrorInvalidArgument 

    -- rand

    FApp HiFunRand args -> traverse evalT args >>= \case
        [NVal l, NVal r] -> do
            li <- readHiInt l
            ri <- readHiInt r
            when (max ri li > fromIntegral (maxBound @Int) || min ri li < fromIntegral (minBound @Int)) $ throwError HiErrorInvalidArgument
            return $ HiValueAction $ HiActionRand (fromInteger li) (fromInteger ri)

        _           -> throwError HiErrorInvalidArgument 

    -- echo

    FApp HiFunEcho args -> traverse evalT args >>= \case
        [HiValueString s] -> return $ HiValueAction $ HiActionEcho s
        _                 -> throwError HiErrorInvalidArgument 

    -- FApp x y -> error "asd"

    FApp _ _             -> throwError HiErrorArityMismatch

    HiExprApply exp@(HiExprApply _ _) args2 -> do 
        f <- evalT exp
        evalT $ HiExprApply (HiExprValue f) args2

    HiExprApply _ _ -> throwError HiErrorInvalidFunction
        
evalEq :: HiMonad m => HiExpr -> HiExpr -> EvalT m Bool
evalEq l r = (==) <$> evalT l <*> evalT r

evalLt :: HiMonad m => HiExpr -> HiExpr -> EvalT m Bool
evalLt (HiExprValue (HiValueBool _)) (NExp _) = pure True
evalLt (NExp _) (HiExprValue (HiValueBool _)) = pure False
evalLt l                             r        = (<) <$> evalT l <*> evalT r

readHiInt :: (Integral i, HiMonad m) => Rational -> EvalT m i
readHiInt = maybe (throwError HiErrorInvalidArgument) return . ratToInt

class HiContainer c where
    cLength :: c -> Int
    cPack   :: c -> HiValue
    cIndex  :: c -> Int -> HiValue 
    cTake   :: Int -> c -> c
    cDrop   :: Int -> c -> c

instance HiContainer Text.Text where
    cLength  = Text.length
    cPack    = HiValueString
    cIndex c = cPack . Text.singleton . Text.index c
    cTake    = Text.take 
    cDrop    = Text.drop

instance HiContainer (Sequence.Seq HiValue) where
    cLength = Sequence.length
    cPack   = HiValueList 
    cIndex  = Sequence.index
    cTake   = Sequence.take
    cDrop   = Sequence.drop

instance HiContainer C8ByteString.ByteString where
    cLength = C8ByteString.length
    cPack   = HiValueBytes 
    cIndex i = HiValueNumber . fromIntegral . ord . C8ByteString.index i
    cTake   = C8ByteString.take
    cDrop   = C8ByteString.drop

evalContainer :: (HiMonad m, HiContainer c) => c -> [HiExpr] -> EvalT m HiValue
evalContainer c args = traverse evalT args >>= \case

    [NVal n] -> do
        i <- readHiInt n
        return $ if i >= cLength c || i < 0
                 then HiValueNull
                 else cIndex c i

    [HiValueNull, HiValueNull] -> return $ cPack c

    [HiValueNull, NVal n] -> do
        i <- readHiInt n
        let t = if i < 0 then i `mod` cLength c else i
        return $ cPack $ cTake t c

    [NVal n, HiValueNull] -> do
        i <- readHiInt n
        return . cPack $ cDrop i c

    [NVal from, NVal to] -> do 
        from <- readHiInt from
        to   <- readHiInt to
        let to' = if to < 0 then to `mod` cLength c else to
        return $ cPack $ cTake (to' - from) $ cDrop from c

    _      -> throwError HiErrorInvalidArgument

-- unpackBytesHex :: ByteString.ByteString -> [Int]
-- unpackBytesHex = map ord . ByteString.unpack

-- unpackBytesDec :: C8ByteString.ByteString -> [Int]
-- unpackBytesDec = f . C8ByteString.unpack
--     where f (a:b:rest) = (fst $ head $ readHex [a,b]) : f rest
--           f _          = []

compress :: C8ByteString.ByteString -> C8ByteString.ByteString
compress = LByteString.toStrict 
         . Zlib.compressWith Zlib.defaultCompressParams {Zlib.compressLevel = Zlib.bestCompression} 
         . LByteString.fromStrict
