{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}

-- | Peek and poke functions for network byte order.

module Network.ByteOrder (
    -- *Types
    Buffer
  , Offset
  , BufferSize
  , BufferOverrun(..)
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
  , peekByteString
    -- *From Word to ByteString
  , bytestring8
  , bytestring16
  , bytestring32
  , bytestring64
    -- *From ByteString to Word
  , word8
  , word16
  , word32
  , word64
    -- *Utilities
  , unsafeWithByteString
  , copy
  , bufferIO
    -- *Class to read a buffer
  , Readable(..)
    -- *Reading from buffer
  , ReadBuffer
  , newReadBuffer
  , withReadBuffer
  , read16
  , read24
  , read32
  , extractByteString
  , extractShortByteString
    -- *Writing to buffer
  , WriteBuffer(..)
  , newWriteBuffer
  , withWriteBuffer
  , withWriteBuffer'
  , write8
  , write16
  , write24
  , write32
  , copyByteString
  , copyShortByteString
  , shiftLastN
  , toByteString
  , toShortByteString
  , currentOffset
    -- *Re-exporting
  , Word8, Word16, Word32, Word64, ByteString
  ) where

import Control.Exception (bracket, throwIO, Exception)
import Data.Bits (shiftR, shiftL, (.&.), (.|.))
import Data.ByteString.Internal (ByteString(..), create, memcpy, ByteString(..), unsafeCreate)
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short.Internal as Short
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Typeable
import Data.Word (Word8, Word8, Word16, Word32, Word64)
import Foreign.ForeignPtr (withForeignPtr, newForeignPtr_)
import Foreign.Marshal.Alloc
import Foreign.Ptr (Ptr, plusPtr, plusPtr, minusPtr)
import Foreign.Storable (peek, poke, poke, peek)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- $setup
-- >>> import Data.ByteString hiding (foldl')
-- >>> import Data.Word
-- >>> import Data.List

----------------------------------------------------------------

-- | A pointer to 'Word8'.
type Buffer = Ptr Word8
-- | Offset from the current pointer.
type Offset = Int
-- | Size of a buffer.
type BufferSize = Int

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

peekByteString :: Buffer -> Int -> IO ByteString
peekByteString src len = create len $ \dst -> memcpy dst src len

----------------------------------------------------------------

-- |
--
-- >>> let w = 5 :: Word8
-- >>> unpack $ bytestring8 w
-- [5]
bytestring8 :: Word8 -> ByteString
bytestring8 w = unsafeCreate 1 $ \ptr -> poke8 w ptr 0

-- |
--
-- >>> let w = foldl' (\x y -> x * 256 + y) 0 [5,6] :: Word16
-- >>> unpack $ bytestring16 w
-- [5,6]
bytestring16 :: Word16 -> ByteString
bytestring16 w = unsafeCreate 2 $ \ptr -> poke16 w ptr 0

-- |
--
-- >>> let w = foldl' (\x y -> x * 256 + y) 0 [5,6,7,8] :: Word32
-- >>> unpack $ bytestring32 w
-- [5,6,7,8]
bytestring32 :: Word32 -> ByteString
bytestring32 w = unsafeCreate 4 $ \ptr -> poke32 w ptr 0

-- |
--
-- >>> let w = foldl' (\x y -> x * 256 + y) 0 [1,2,3,4,5,6,7,8] :: Word64
-- >>> unpack $ bytestring64 w
-- [1,2,3,4,5,6,7,8]
bytestring64 :: Word64 -> ByteString
bytestring64 w = unsafeCreate 8 $ \ptr -> poke64 w ptr 0

----------------------------------------------------------------

-- |
--
-- >>> let buf = pack [1,2,3,4,5,6,7,8]
-- >>> word8 buf
-- 1
word8 :: ByteString -> Word8
word8 bs = unsafeDupablePerformIO $ unsafeWithByteString bs peek8

-- |
--
-- >>> let buf = pack [1,2,3,4,5,6,7,8]
-- >>> word16 buf
-- 258
word16 :: ByteString -> Word16
word16 bs = unsafeDupablePerformIO $ unsafeWithByteString bs peek16

-- |
--
-- >>> let buf = pack [1,2,3,4,5,6,7,8]
-- >>> word32 buf
-- 16909060
word32 :: ByteString -> Word32
word32 bs = unsafeDupablePerformIO $ unsafeWithByteString bs peek32

-- |
--
-- >>> let buf = pack [1,2,3,4,5,6,7,8]
-- >>> word64 buf
-- 72623859790382856
word64 :: ByteString -> Word64
word64 bs = unsafeDupablePerformIO $ unsafeWithByteString bs peek64

----------------------------------------------------------------

-- | Using 'ByteString' as 'Buffer' and call the 'IO' action
--   of the second argument by passing the start point and the offset
--   of the 'ByteString'.
--   Note that if a 'ByteString' is created newly, its offset is 0.
unsafeWithByteString :: ByteString -> (Buffer -> Offset -> IO a) -> IO a
unsafeWithByteString (PS fptr off _) io = withForeignPtr fptr $
    \ptr -> io ptr off

-- | Copying the bytestring to the buffer.
--   This function returns the point where the next copy should start.
--
-- >>> let buf = "abc" :: ByteString
-- >>> unsafeWithByteString buf $ \ptr _ -> Network.ByteOrder.copy ptr "ABC" >> return buf
-- "ABC"
copy :: Buffer -> ByteString -> IO Buffer
copy ptr (PS fp o l) = withForeignPtr fp $ \p -> do
    memcpy ptr (p `plusPtr` o) (fromIntegral l)
    return $ ptr `plusPtr` l
{-# INLINE copy #-}

-- | Converting the part of buffer to 'ByteString' and executing the
--   action with it.
--
-- >>> let buf = "abcdef" :: ByteString
-- >>> unsafeWithByteString buf $ \ptr _-> bufferIO ptr 2 return
-- "ab"
bufferIO :: Buffer -> Int -> (ByteString -> IO a) -> IO a
bufferIO ptr siz io = do
    fptr <- newForeignPtr_ ptr
    io $ PS fptr 0 siz

----------------------------------------------------------------

-- | Read and write buffer.
data WriteBuffer = WriteBuffer {
    start :: Buffer
  , limit :: Buffer
  , offset :: IORef Buffer
  , oldoffset :: IORef Buffer
  }

-- | Creating a write buffer with the given buffer.
newWriteBuffer :: Buffer -> BufferSize -> IO WriteBuffer
newWriteBuffer buf siz =
    WriteBuffer buf (buf `plusPtr` siz) <$> newIORef buf <*> newIORef buf

{-# INLINE write8 #-}
-- | Write one byte and ff one byte.
--   If buffer overrun occurs, 'BufferOverrun' is thrown.
write8 :: WriteBuffer -> Word8 -> IO ()
write8 WriteBuffer{..} w = do
    ptr <- readIORef offset
    if ptr >= limit then
        throwIO BufferOverrun
      else do
        poke ptr w
        let ptr' = ptr `plusPtr` 1
        writeIORef offset ptr'

{-# INLINE write16 #-}
-- | Write two bytes and ff one byte.
--   If buffer overrun occurs, 'BufferOverrun' is thrown.
write16 :: WriteBuffer -> Word16 -> IO ()
write16 WriteBuffer{..} w = do
    ptr <- readIORef offset
    if ptr `plusPtr` 1 >= limit then
        throwIO BufferOverrun
      else do
        poke16 w ptr 0
        let ptr' = ptr `plusPtr` 2
        writeIORef offset ptr'

{-# INLINE write24 #-}
-- | Write three bytes and ff one byte.
--   If buffer overrun occurs, 'BufferOverrun' is thrown.
write24 :: WriteBuffer -> Word32 -> IO ()
write24 WriteBuffer{..} w = do
    ptr <- readIORef offset
    if ptr `plusPtr` 2 >= limit then
        throwIO BufferOverrun
      else do
        poke24 w ptr 0
        let ptr' = ptr `plusPtr` 3
        writeIORef offset ptr'

{-# INLINE write32 #-}
-- | Write four bytes and ff one byte.
--   If buffer overrun occurs, 'BufferOverrun' is thrown.
write32 :: WriteBuffer -> Word32 -> IO ()
write32 WriteBuffer{..} w = do
    ptr <- readIORef offset
    if ptr `plusPtr` 3 >= limit then
        throwIO BufferOverrun
      else do
        poke32 w ptr 0
        let ptr' = ptr `plusPtr` 4
        writeIORef offset ptr'

{-# INLINE shiftLastN #-}
-- | Shifting the N-bytes area just before the current pointer.
--   'Offset' is the distance from the offset pointer.
--   If 'Offset' is positive, shift it to right.
--   If 'Offset' is negative, shift it to left.
shiftLastN :: WriteBuffer -> Offset -> Int -> IO ()
shiftLastN WriteBuffer{..} 0 _   = return ()
shiftLastN WriteBuffer{..} i len = do
    ptr <- readIORef offset
    let ptr' = ptr `plusPtr` i
    if ptr' >= limit then
        throwIO BufferOverrun
      else if i < 0 then do
        let src = ptr `plusPtr` negate len
            dst = src `plusPtr` i
        shiftLeft dst src len
        writeIORef offset ptr'
      else do
        let src = ptr `plusPtr` (-1)
            dst = ptr' `plusPtr` (-1)
        shiftRight dst src len
        writeIORef offset ptr'
  where
    -- memcpy cannot be used for overlapped areas.
    shiftLeft :: Buffer -> Buffer -> Int -> IO ()
    shiftLeft _    _    0   = return ()
    shiftLeft dst src n = do
        peek src >>= poke dst
        shiftLeft (dst `plusPtr` 1) (src `plusPtr` 1) (n - 1)
    shiftRight :: Buffer -> Buffer -> Int -> IO ()
    shiftRight _    _    0   = return ()
    shiftRight dst src n = do
        peek src >>= poke dst
        shiftRight (dst `plusPtr` (-1)) (src `plusPtr` (-1)) (n - 1)

{-# INLINE copyByteString #-}
-- | Copy the content of 'ByteString' and ff its length.
--   If buffer overrun occurs, 'BufferOverrun' is thrown.
copyByteString :: WriteBuffer -> ByteString -> IO ()
copyByteString WriteBuffer{..} (PS fptr off len) = withForeignPtr fptr $ \ptr -> do
    let src = ptr `plusPtr` off
    dst <- readIORef offset
    let dst' = dst `plusPtr` len
    if dst' > limit then
        throwIO BufferOverrun
      else do
        memcpy dst src len
        writeIORef offset dst'

-- | Copy the content of 'ShortByteString' and ff its length.
--   If buffer overrun occurs, 'BufferOverrun' is thrown.
copyShortByteString :: WriteBuffer -> ShortByteString -> IO ()
copyShortByteString WriteBuffer{..} sbs = do
    dst <- readIORef offset
    let len = Short.length sbs
    let dst' = dst `plusPtr` len
    if dst' > limit then
        throwIO BufferOverrun
      else do
        Short.copyToPtr sbs 0 dst len
        writeIORef offset dst'

-- | Copy the area from 'start' to the current pointer to 'ByteString'.
toByteString :: WriteBuffer -> IO ByteString
toByteString WriteBuffer{..} = do
    ptr <- readIORef offset
    let len = ptr `minusPtr` start
    create len $ \p -> memcpy p start len

-- | Copy the area from 'start' to the current pointer to 'ShortByteString'.
toShortByteString :: WriteBuffer -> IO ShortByteString
toShortByteString WriteBuffer{..} = do
    ptr <- readIORef offset
    let len = ptr `minusPtr` start
    Short.createFromPtr start len

-- | Allocate a temporary buffer and copy the result to 'ByteString'.
--
-- >>> withWriteBuffer 1 $ \wbuf -> write8 wbuf 65
-- "A"
-- >>> withWriteBuffer 3 $ \wbuf -> copyByteString wbuf "ABC"
-- "ABC"
-- >>> withWriteBuffer 5 $ \wbuf -> copyShortByteString wbuf "ABCEF"
-- "ABCEF"
withWriteBuffer :: BufferSize -> (WriteBuffer -> IO ()) -> IO ByteString
withWriteBuffer siz action = bracket (mallocBytes siz) free $ \buf -> do
    wbuf <- newWriteBuffer buf siz
    action wbuf
    toByteString wbuf

-- | Allocate a temporary buffer and copy the result to 'ByteString' with
--   an additional value.
--
-- >>> withWriteBuffer' 1 $ \wbuf -> write8 wbuf 65 >> return 'a'
-- ("A",'a')
withWriteBuffer' :: BufferSize -> (WriteBuffer -> IO a) -> IO (ByteString, a)
withWriteBuffer' siz action = bracket (mallocBytes siz) free $ \buf -> do
    wbuf <- newWriteBuffer buf siz
    x <- action wbuf
    bs <- toByteString wbuf
    return (bs,x)

{-# INLINE currentOffset #-}
-- | Getting the offset pointer.
currentOffset :: WriteBuffer -> IO Buffer
currentOffset WriteBuffer{..} = readIORef offset

----------------------------------------------------------------

class Readable a where
    -- | Reading one byte as 'Word8' and ff one byte.
    read8 :: a -> IO Word8
    -- | Reading one byte as 'Int' and ff one byte. If buffer overrun occurs, -1 is returned.
    readInt8 :: a -> IO Int
    -- | Fast forward the offset pointer. The boundary is not checked.
    ff :: a -> Offset -> IO ()
    -- | Returning the length of the remaining
    remainingSize :: a -> IO Int
    -- | Executing an action on the current offset pointer.
    withCurrentOffSet :: a -> (Buffer -> IO b) -> IO b
    -- | Memorizing the current offset pointer.
    save :: a -> IO ()
    -- | Getting how many bytes from the saved offset pinter.
    savingSize :: a -> IO Int

instance Readable WriteBuffer where
    {-# INLINE read8 #-}
    read8 WriteBuffer{..} = do
        ptr <- readIORef offset
        w <- peek ptr
        writeIORef offset $ ptr `plusPtr` 1
        return w
    {-# INLINE readInt8 #-}
    readInt8 WriteBuffer{..} = do
        ptr <- readIORef offset
        if ptr < limit then do
            w <- peek ptr
            writeIORef offset $ ptr `plusPtr` 1
            let i = fromIntegral w
            return i
          else
            return (-1)
    {-# INLINE ff #-}
    ff WriteBuffer{..} n = do
        ptr <- readIORef offset
        let ptr' = ptr `plusPtr` n
        writeIORef offset ptr'
    {-# INLINE remainingSize #-}
    remainingSize WriteBuffer{..} = do
        ptr <- readIORef offset
        return $ limit `minusPtr` ptr
    {-# INLINE withCurrentOffSet #-}
    withCurrentOffSet WriteBuffer{..} action = readIORef offset >>= action
    {-# INLINE save #-}
    save WriteBuffer{..} = readIORef offset >>= writeIORef oldoffset
    {-# INLINE savingSize #-}
    savingSize WriteBuffer{..} = do
        old <- readIORef oldoffset
        cur <- readIORef offset
        return $ cur `minusPtr` old

instance Readable ReadBuffer where
    {-# INLINE read8 #-}
    read8 (ReadBuffer w) = read8 w
    {-# INLINE readInt8 #-}
    readInt8 (ReadBuffer w) = readInt8 w
    {-# INLINE ff #-}
    ff (ReadBuffer w) = ff w
    {-# INLINE remainingSize #-}
    remainingSize (ReadBuffer w) = remainingSize w
    {-# INLINE withCurrentOffSet #-}
    withCurrentOffSet (ReadBuffer w) = withCurrentOffSet w
    {-# INLINE save #-}
    save (ReadBuffer w) = save w
    {-# INLINE savingSize #-}
    savingSize (ReadBuffer w) = savingSize w

----------------------------------------------------------------

-- | Read only buffer. To ensure that the internal is not modified,
--   this is an abstract data type.
newtype ReadBuffer = ReadBuffer WriteBuffer

-- | Creating a read buffer with the given buffer.
newReadBuffer :: Buffer -> BufferSize -> IO ReadBuffer
newReadBuffer buf siz = ReadBuffer <$> newWriteBuffer buf siz

-- | Converting 'ByteString' to 'ReadBuffer' and run the action
--   with it.
withReadBuffer :: ByteString -> (ReadBuffer -> IO a) -> IO a
withReadBuffer (PS fp off siz) action = withForeignPtr fp $ \ptr -> do
    let buf = ptr `plusPtr` off
    nsrc <- newReadBuffer buf siz
    action nsrc

-- | Extracting 'ByteString' from the current offset.
--   The contents is copied, not shared.
--   Its length is specified by the 2nd argument.
--   If the length is positive, the area after the current pointer is extracted and FF the length finally.
--   If the length is negative, the area before the current pointer is extracted and does not FF.
extractByteString :: Readable a => a -> Int -> IO ByteString
extractByteString wbuf len
  | len == 0 = return mempty
  | len >  0 = do
    bs <- withCurrentOffSet wbuf $ \src ->
        create len $ \dst -> memcpy dst src len
    ff wbuf len
    return bs
  | otherwise = withCurrentOffSet wbuf $ \src0 -> do
      let src = src0 `plusPtr` len
      let len' = negate len
      create len' $ \dst -> memcpy dst src len'

-- | Extracting 'ShortByteString' from the current offset.
--   The contents is copied, not shared.
--   Its length is specified by the 2nd argument.
--   If the length is positive, the area after the current pointer is extracted and FF the length finally.
--   If the length is negative, the area before the current pointer is extracted and does not FF.
extractShortByteString :: Readable a => a -> Int -> IO ShortByteString
extractShortByteString wbuf len
  | len == 0 = return mempty
  | len >  0 = do
    sbs <- withCurrentOffSet wbuf $ \src -> Short.createFromPtr src len
    ff wbuf len
    return sbs
  | otherwise = withCurrentOffSet wbuf $ \src0 -> do
      let src = src0 `plusPtr` len
      let len' = negate len
      Short.createFromPtr src len'

-- | Reading two bytes as 'Word16' and ff two bytes.
read16 :: Readable a => a -> IO Word16
read16 rbuf = do
    w16 <- withCurrentOffSet rbuf (`peek16` 0)
    ff rbuf 2
    return w16

-- | Reading three bytes as 'Word32' and ff three bytes.
read24 :: Readable a => a -> IO Word32
read24 rbuf = do
    w24 <- withCurrentOffSet rbuf (`peek24` 0)
    ff rbuf 3
    return w24

-- | Reading four bytes as 'Word32' and ff four bytes.
read32 :: Readable a => a -> IO Word32
read32 rbuf = do
    w32 <- withCurrentOffSet rbuf (`peek32` 0)
    ff rbuf 4
    return w32

-- | Buffer overrun exception.
data BufferOverrun = BufferOverrun -- ^ The buffer size is not enough
                     deriving (Eq,Show,Typeable)

instance Exception BufferOverrun
