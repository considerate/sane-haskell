{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Numeric.Array.Simple
  ( Array (..),
    encodeArray,
    decodeArray,
  )
where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString
import qualified Data.ByteString.Lazy as Lazy
import Data.Foldable (traverse_)
import Data.Int (Int32, Int64)
import qualified Data.Serialize.Get as Get
import qualified Data.Serialize.IEEE754 as Floats
import qualified Data.Serialize.Put as Put
import Data.String (fromString)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as Vector
import Data.Word (Word32, Word64, Word8)
import Foreign (castForeignPtr, sizeOf)
import System.Endian (Endianness (..), getSystemEndianness)

type Dim = Int

data Array
  = Float32Array [Dim] (Vector Float)
  | Int32Array [Dim] (Vector Int32)
  | UInt32Array [Dim] (Vector Word32)
  | Float64Array [Dim] (Vector Double)
  | Int64Array [Dim] (Vector Int64)
  | UInt64Array [Dim] (Vector Word64)
  deriving (Show)

encodeArray :: Array -> Lazy.ByteString
encodeArray a = Put.runPutLazy $ do
  Put.putShortByteString (fromString "SANE")
  putArray a

putWith :: Vector.Storable a => (a -> Put.Put) -> Word64 -> Vector a -> Put.Put
putWith p len xs = do
  Put.putWord64le $ len * fromIntegral (sizeOfElem xs)
  Vector.mapM_ p xs

putShape :: [Word64] -> Put.Put
putShape ds = do
  Put.putWord32le (fromIntegral $ length ds)
  traverse_ (Put.putWord64le) ds

putVectorWith :: (Vector.Storable a) => (a -> Put.Put) -> [Dim] -> Word8 -> Vector a -> Put.PutM ()
putVectorWith p shape dataType xs = do
  let shape' = fromIntegral <$> shape
  putShape shape'
  Put.putWord8 dataType
  putWith p (product shape') xs

putArray :: Array -> Put.Put
putArray array@(Float32Array shape xs) =
  putVectorWith Floats.putFloat32le shape (arrayDataType array) xs
putArray array@(Int32Array shape xs) =
  putVectorWith Put.putInt32le shape (arrayDataType array) xs
putArray array@(UInt32Array shape xs) =
  putVectorWith Put.putWord32le shape (arrayDataType array) xs
putArray array@(Float64Array shape xs) =
  putVectorWith Floats.putFloat64le shape (arrayDataType array) xs
putArray array@(Int64Array shape xs) =
  putVectorWith Put.putInt64le shape (arrayDataType array) xs
putArray array@(UInt64Array shape xs) =
  putVectorWith Put.putWord64le shape (arrayDataType array) xs

getN :: Word32 -> Get.Get a -> Get.Get [a]
getN n0 p = go [] n0
  where
    go acc 0 = pure $! reverse acc
    go acc n = do
      x <- p
      x `seq` go (x : acc) (n - 1)

sizeOfElem :: forall a proxy. (Vector.Storable a) => proxy a -> Int
sizeOfElem p = sizeOf (undefined :: a)

castBytes :: (Vector.Storable a) => ByteString -> Vector a
castBytes bytes = vec
  where
    vec = Vector.unsafeFromForeignPtr (castForeignPtr fptr) (scale off) (scale len)
    (fptr, off, len) = ByteString.toForeignPtr bytes
    scale = (`div` sizeOfElem vec)

toVector :: Vector.Storable a => Word64 -> Get.Get a -> ByteString -> Get.Get (Vector a)
toVector len p bytes = case getSystemEndianness of
  LittleEndian -> pure $ castBytes bytes
  BigEndian -> getVector
    where
      getVector = Vector.replicateM (fromIntegral len) p

arrayDataType :: Array -> Word8
arrayDataType Float32Array {} = 0
arrayDataType Int32Array {} = 1
arrayDataType UInt32Array {} = 2
arrayDataType Float64Array {} = 3
arrayDataType Int64Array {} = 4
arrayDataType UInt64Array {} = 5

buildArray :: Word64 -> Word8 -> [Dim] -> ByteString -> Get.Get Array
buildArray len dataType shape bytes =
  case dataType of
    0 -> Float32Array shape <$> toVector len Floats.getFloat32le bytes
    1 -> Int32Array shape <$> toVector len Get.getInt32le bytes
    2 -> UInt32Array shape <$> toVector len Get.getWord32le bytes
    3 -> Float64Array shape <$> toVector len Floats.getFloat64le bytes
    4 -> Int64Array shape <$> toVector len Get.getInt64le bytes
    5 -> UInt64Array shape <$> toVector len Get.getWord64le bytes
    _ -> fail $ "Use of reserved datatype: " <> show dataType

getArray :: Get.Get Array
getArray = do
  magic <- Get.getBytes 4
  unless (magic == fromString "SANE") $
    fail ("Not a SANE encoded array, got header:" <> show magic)
  shapeLen <- Get.getWord32le
  shape <- getN shapeLen Get.getWord64le
  dataType <- Get.getWord8
  dataLen <- Get.getWord64le
  dataBytes <- Get.getBytes (fromIntegral dataLen)
  buildArray dataLen dataType (fromIntegral <$> shape) dataBytes

decodeArray :: ByteString -> Either String Array
decodeArray = Get.runGet getArray
