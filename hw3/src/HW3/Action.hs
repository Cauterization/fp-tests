{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
module HW3.Action where

import HW3.Base
import Control.Exception
import qualified Data.Set as Set
import qualified Data.Sequence as Sequence
import Control.Monad.Reader
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import System.Directory
import qualified Data.ByteString as ByteString
import System.IO
import Data.Functor (($>))
import qualified Data.Time as Time
import System.Random (randomRIO)
import qualified Data.Text.IO as Text

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord, Bounded, Enum)

data PermissionException =
  PermissionRequired HiPermission
  deriving (Show, Exception)

newtype HIO a = HIO { runHIO :: Set.Set HiPermission -> IO a } 
    deriving (Functor, Applicative, Monad) via ReaderT (Set.Set HiPermission) IO

instance HiMonad HIO where
    runAction = \case
        
        HiActionCwd -> withRequirements [AllowRead] $ do
            cd <- getCurrentDirectory
            -- (HiValueString . Text.pack . concat) <$> listDirectory cd
            return $ HiValueString $ Text.pack cd

        HiActionChDir d -> withRequirements [AllowRead] $ HiValueNull <$ do
            setCurrentDirectory d 

        HiActionRead f -> withRequirements [AllowRead] $ do
            b <- doesFileExist f
            if b
            then withFile f ReadMode $ \handle -> do
                bs <- ByteString.hGetContents handle
                case Text.decodeUtf8' bs of
                    Left _  -> return $ HiValueBytes bs 
                    Right s -> return $ HiValueString s
            else do
                dirs <- filterM (doesDirectoryExist) <$> listDirectory f
                HiValueList . Sequence.fromList . map (HiValueString . Text.pack) <$> dirs

        HiActionWrite f bs -> withRequirements [AllowWrite] $ withFile f WriteMode $ 
            \handle -> ByteString.hPut handle bs $> HiValueNull

        HiActionMkDir f -> withRequirements [AllowWrite] $ HiValueNull <$ createDirectoryIfMissing False f

        HiActionNow -> withRequirements [AllowTime] $ HiValueTime <$> Time.getCurrentTime

        HiActionRand l r -> withRequirements [] $ HiValueNumber . fromIntegral <$> randomRIO (l, r)

        HiActionEcho s -> withRequirements [AllowWrite] $ HiValueNull <$ Text.putStrLn s

        -- _ -> undefined

withRequirements :: [HiPermission] -> IO a -> HIO a
withRequirements perms action = HIO $ \allowed -> do
    forM_ perms $ \p -> unless (p `Set.member` allowed) $ throw $ PermissionRequired p
    liftIO action

