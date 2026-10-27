# ChangeLog for network-byte-order

## 0.1.9

* Security: `word8`, `word16`, `word32` and `word64` read beyond the
  given `ByteString` if it was too short. Now `BufferOverrun` is thrown.
* Security: `extractByteString` and `extractShortByteString` with a
  negative length read before the beginning of the buffer.
  Now `BufferOverrun` is thrown.
