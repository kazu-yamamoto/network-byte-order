module Network.ByteOrder (
    -- *Types
    Buffer
  , Offset
    -- *Poking
  , poke8
  , poke16
  , poke24
  , poke32
  , poke64
    -- *Peeking
  , peek8
  , peek16
  , peek24
  , peek32
  , peek64
    -- *Creating ByteString
  , bytestring1
  , bytestring2
  , bytestring4
  , bytestring8
    -- *Utilities
  , unsafeWithByteString
  ) where

import Data.Bits (shiftR, shiftL, (.&.), (.|.))
import Data.ByteString.Internal (ByteString(..), unsafeCreate)
import Data.Word (Word8, Word16, Word32, Word64)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (poke, peek)
import Foreign.ForeignPtr (withForeignPtr)

-- $setup
-- >>> import Data.ByteString

----------------------------------------------------------------

type Buffer = Ptr Word8
type Offset = Int

----------------------------------------------------------------

(+.) :: Buffer -> Offset -> Buffer
(+.) = plusPtr

----------------------------------------------------------------

-- |
--
-- >>> let buf = pack [1,2,3,4]
-- >>> unsafeWithByteString buf (poke8 0)
-- >>> unpack buf
-- [0,2,3,4]
poke8 :: Word8 -> Buffer -> Offset -> IO ()
poke8 w ptr off = poke (ptr +. off) w

-- |
--
-- >>> let buf = pack [1,2,3,4]
-- >>> unsafeWithByteString buf (poke16 (7*256 + 8))
-- >>> unpack buf
-- [7,8,3,4]
poke16 :: Word16 -> Buffer -> Offset -> IO ()
poke16 w ptr off = do
    poke8 w0 ptr off
    poke8 w1 ptr (off + 1)
  where
    w0 = fromIntegral ((w `shiftR`  8) .&. 0xff)
    w1 = fromIntegral  (w              .&. 0xff)

-- |
--
-- >>> let buf = pack [1,2,3,4]
-- >>> unsafeWithByteString buf (poke24 (6*65536 + 7*256 + 8))
-- >>> unpack buf
-- [6,7,8,4]
poke24 :: Word32 -> Buffer -> Offset -> IO ()
poke24 w ptr off = do
    poke8 w0 ptr off
    poke8 w1 ptr (off + 1)
    poke8 w2 ptr (off + 2)
  where
    w0 = fromIntegral ((w `shiftR` 16) .&. 0xff)
    w1 = fromIntegral ((w `shiftR`  8) .&. 0xff)
    w2 = fromIntegral  (w              .&. 0xff)

-- |
--
-- >>> let buf = pack [1,2,3,4]
-- >>> unsafeWithByteString buf (poke32 (6*65536 + 7*256 + 8))
-- >>> unpack buf
-- [0,6,7,8]
poke32 :: Word32 -> Buffer -> Offset -> IO ()
poke32 w ptr off = do
    poke8 w0 ptr off
    poke8 w1 ptr (off + 1)
    poke8 w2 ptr (off + 2)
    poke8 w3 ptr (off + 3)
  where
    w0 = fromIntegral ((w `shiftR` 24) .&. 0xff)
    w1 = fromIntegral ((w `shiftR` 16) .&. 0xff)
    w2 = fromIntegral ((w `shiftR`  8) .&. 0xff)
    w3 = fromIntegral  (w              .&. 0xff)

-- |
--
-- >>> let buf = pack [1,2,3,4,5,6,7,8]
-- >>> unsafeWithByteString buf (poke64 (6*65536 + 7*256 + 8))
-- >>> unpack buf
-- [0,0,0,0,0,6,7,8]
poke64 :: Word64 -> Buffer -> Offset -> IO ()
poke64 w ptr off = do
    poke8 w0 ptr off
    poke8 w1 ptr (off + 1)
    poke8 w2 ptr (off + 2)
    poke8 w3 ptr (off + 3)
    poke8 w4 ptr (off + 4)
    poke8 w5 ptr (off + 5)
    poke8 w6 ptr (off + 6)
    poke8 w7 ptr (off + 7)
  where
    w0 = fromIntegral ((w `shiftR` 56) .&. 0xff)
    w1 = fromIntegral ((w `shiftR` 48) .&. 0xff)
    w2 = fromIntegral ((w `shiftR` 40) .&. 0xff)
    w3 = fromIntegral ((w `shiftR` 32) .&. 0xff)
    w4 = fromIntegral ((w `shiftR` 24) .&. 0xff)
    w5 = fromIntegral ((w `shiftR` 16) .&. 0xff)
    w6 = fromIntegral ((w `shiftR`  8) .&. 0xff)
    w7 = fromIntegral  (w              .&. 0xff)

----------------------------------------------------------------

-- |
--
-- >>> let buf = pack [1,2,3,4]
-- >>> unsafeWithByteString buf peek8
-- 1
peek8 :: Buffer -> Offset -> IO Word8
peek8 ptr off = peek (ptr +. off)

-- |
--
-- >>> let buf = pack [1,2,3,4]
-- >>> unsafeWithByteString buf peek16
-- 258
peek16 :: Buffer -> Offset -> IO Word16
peek16 ptr off = do
    w0 <- (`shiftL` 8) . fromIntegral <$> peek8 ptr off
    w1 <-                fromIntegral <$> peek8 ptr (off + 1)
    return $ w0 .|. w1

-- |
--
-- >>> let buf = pack [1,2,3,4]
-- >>> unsafeWithByteString buf peek24
-- 66051
peek24 :: Buffer -> Offset -> IO Word32
peek24 ptr off = do
    w0 <- (`shiftL` 16) . fromIntegral <$> peek8 ptr off
    w1 <- (`shiftL`  8) . fromIntegral <$> peek8 ptr (off + 1)
    w2 <-                 fromIntegral <$> peek8 ptr (off + 2)
    return $ w0 .|. w1 .|. w2

-- |
--
-- >>> let buf = pack [1,2,3,4]
-- >>> unsafeWithByteString buf peek32
-- 16909060
peek32 :: Buffer -> Offset -> IO Word32
peek32 ptr off = do
    w0 <- (`shiftL` 24) . fromIntegral <$> peek8 ptr off
    w1 <- (`shiftL` 16) . fromIntegral <$> peek8 ptr (off + 1)
    w2 <- (`shiftL`  8) . fromIntegral <$> peek8 ptr (off + 2)
    w3 <-                 fromIntegral <$> peek8 ptr (off + 3)
    return $ w0 .|. w1 .|. w2 .|. w3

-- |
--
-- >>> let buf = pack [1,2,3,4,5,6,7,8]
-- >>> unsafeWithByteString buf peek64
-- 72623859790382856
peek64 :: Buffer -> Offset -> IO Word64
peek64 ptr off = do
    w0 <- (`shiftL` 56) . fromIntegral <$> peek8 ptr off
    w1 <- (`shiftL` 48) . fromIntegral <$> peek8 ptr (off + 1)
    w2 <- (`shiftL` 40) . fromIntegral <$> peek8 ptr (off + 2)
    w3 <- (`shiftL` 32) . fromIntegral <$> peek8 ptr (off + 3)
    w4 <- (`shiftL` 24) . fromIntegral <$> peek8 ptr (off + 4)
    w5 <- (`shiftL` 16) . fromIntegral <$> peek8 ptr (off + 5)
    w6 <- (`shiftL`  8) . fromIntegral <$> peek8 ptr (off + 6)
    w7 <-                 fromIntegral <$> peek8 ptr (off + 7)
    return $ w0 .|. w1 .|. w2 .|. w3 .|. w4 .|. w5 .|. w6 .|. w7

----------------------------------------------------------------

bytestring1 :: Word8 -> ByteString
bytestring1 w = unsafeCreate 1 $ \ptr -> poke8 w ptr 0

bytestring2 :: Word16 -> ByteString
bytestring2 w = unsafeCreate 2 $ \ptr -> poke16 w ptr 0

bytestring4 :: Word32 -> ByteString
bytestring4 w = unsafeCreate 4 $ \ptr -> poke32 w ptr 0

bytestring8 :: Word64 -> ByteString
bytestring8 w = unsafeCreate 8 $ \ptr -> poke64 w ptr 0

----------------------------------------------------------------

-- | Using 'ByteString' as 'Buffer' and call the 'IO' action
--   of the second argument by passing the start point and the offset
--   of the 'ByteString'.
--   Note that if a 'ByteString' is created newly, its offset is 0.
unsafeWithByteString :: ByteString -> (Buffer -> Offset -> IO a) -> IO a
unsafeWithByteString (PS fptr off _) io = withForeignPtr fptr $
    \ptr -> io ptr off
