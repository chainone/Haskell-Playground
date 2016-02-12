import System.Environment
import System.IO
import Data.Digest.Pure.MD5
import Data.List
import qualified Data.ByteString.Lazy as L

sampleLen = 4 * 1024

offsets :: Integer -> Maybe [Integer]
offsets size
         | size < 8 * 1024 = Nothing
         | otherwise = Just [4*1024, (div size 3) * 2, div size 3, size-(8*1024)]

sampleHash :: L.ByteString -> Integer -> MD5Digest
sampleHash bs offset = md5 $ L.take sampleLen $ L.drop (fromIntegral offset) bs

allSampleHash :: L.ByteString -> Integer -> [MD5Digest]
allSampleHash bs size = case offsets size of Nothing -> error "The file size must be larger than 8 KB"
                                             Just os -> fmap (sampleHash bs) os

fileHash :: FilePath -> IO String
fileHash path = do
   handle <- openFile path ReadMode
   size <- hFileSize handle
   content <- L.hGetContents handle
   return $ intercalate ";" $ fmap show $ allSampleHash content size

main = do
   (path:_) <- getArgs
   hashString <- fileHash path
   putStrLn $ "The final hash for file " ++ path ++ " is:"
   putStrLn hashString
