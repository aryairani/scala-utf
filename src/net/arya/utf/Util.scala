package net.arya.utf

/**
 * Created by IntelliJ IDEA.
 * User: arya
 * Date: 12/27/10
 * Time: 10:12 PM
 */


import collection.mutable.{ArrayBuilder, Builder}

object Util {
  val substitutionChar = '�'

  def decodeAt(code: Array[Byte], startOffset: Int): UtfDecodeResult = {

    if ({
      val f1 = (code(startOffset) & 0x7f).asInstanceOf[Byte]
      f1 == code(startOffset)
    }) {
      val b1 = code(startOffset)
      UtfDecodeResult(b1:Int, 1)
    }

    else if ({
      // f1: turn on bits 7&8 (|=0xC0), turn off bit 6 (&=0xDF); and the result should be unchanged
      val f1 = ((code(startOffset)   | 0xC0) & 0xDf).asInstanceOf[Byte]
      // f2: turn on bit 8 (|=0x80), turn off bit 7 (&=0xbf); and the result should be unchanged
      val f2 = ((code(startOffset+1) | 0x80) & 0xBf).asInstanceOf[Byte]

      f1 == code(startOffset) && f2 == code(startOffset+1)

    }) {

      val b1 = (code(startOffset)   & 0x1f) << 6
      val b2 = (code(startOffset+1) & 0x3f)
      UtfDecodeResult(b1|b2:Int, 2)
    }

    else if ({

      // f1: turn on bits 6,7,8 (|=0xE0), turn off bit 5 (&=0xEf); and the result should be unchanged
      val f1 = ((code(startOffset)   | 0xE0) & 0xEf).asInstanceOf[Byte]
      // f2: turn on bit 8 (|=0x80), turn off bit 7 (&=0xbf); and the result should be unchanged
      val f2 = ((code(startOffset+1) | 0x80) & 0xBf).asInstanceOf[Byte]
      // f3: turn on bit 8 (|=0x80), turn off bit 7 (&=0xbf); and the result should be unchanged
      val f3 = ((code(startOffset+2) | 0x80) & 0xBf).asInstanceOf[Byte]

      f1 == code(startOffset) && f2 == code(startOffset+1) && f3 == code(startOffset+2)

    }) {

      val b1 = (code(startOffset)   & 0x0f) << 12
      val b2 = (code(startOffset+1) & 0x3f) << 6
      val b3 = (code(startOffset+2) & 0x3f)
      UtfDecodeResult(b1|b2|b3:Int, 3)
    }

    else if ({
      // f1: turn on bits 5,6,7,8 (|=0xF0), turn off bit 4 (&=0xF7); and the result should be unchanged
      val f1 = ((code(startOffset)   | 0xF0) & 0xF7).asInstanceOf[Byte]
      // f2: turn on bit 8 (|=0x80), turn off bit 7 (&=0xbf); and the result should be unchanged
      val f2 = ((code(startOffset+1) | 0x80) & 0xBf).asInstanceOf[Byte]
      // f3: turn on bit 8 (|=0x80), turn off bit 7 (&=0xbf); and the result should be unchanged
      val f3 = ((code(startOffset+2) | 0x80) & 0xBf).asInstanceOf[Byte]
      // f4: turn on bit 8 (|=0x80), turn off bit 7 (&=0xbf); and the result should be unchanged
      val f4 = ((code(startOffset+3) | 0x80) & 0xBf).asInstanceOf[Byte]

      f1 == code(startOffset) && f2 == code(startOffset+1) && f3 == code(startOffset+2) && f4 == code(startOffset+3)

    }) {

      val b1 = (code(startOffset)   & 0x07) << 18
      val b2 = (code(startOffset+1) & 0x3f) << 12
      val b3 = (code(startOffset+2) & 0x3f) << 6
      val b4 = (code(startOffset+3) & 0x3f)
      UtfDecodeResult(b1|b2|b3|b4:Int, 4)
    }

    else {
      val sb = new StringBuilder
      sb ++= "Invalid UTF-8 sequence at index %d: ".format(startOffset)

      sb ++= (
        for (i <- startOffset to List(startOffset + 4, code.length).min) yield
        {
          "0x%02x".format(code(i))
        }
      ).mkString(", ")

      if (code.length > startOffset + 4)
        sb ++= "..."

      throw new IllegalArgumentException(sb.result)
    }
  }

  def decodeAt(code: Array[Char], startOffset: Int): UtfDecodeResult = {
    if ({
      val f1 = code(startOffset) & 0xFC00  // clear bottom 10 bits

      f1 != 0xD800 // the remaining 6 bits should not be 110110
    }) {
      val s1 = code(startOffset)
      UtfDecodeResult(s1:Int, 1)
    }

    else if ({
      // f1: turn on bits 12,13,15,16, and turn off bits 11 & 14, the result should be unchanged
      val f1 = ((code(startOffset)   | 0xD800) & 0xDbff).asInstanceOf[Char]
      // f1: turn on bits 11,12,13,15,16, and turn off bit 14, the result should be unchanged
      val f2 = ((code(startOffset+1) | 0xDc00) & 0xDfff).asInstanceOf[Char]

      f1 == code(startOffset) && f2 == code(startOffset+1)

    }) {

      val s1 = (code(startOffset)   & 0x3ff) << 10
      val s2 = (code(startOffset+1) & 0x3ff)
      UtfDecodeResult( (s1|s2) + 0x10000, 2)
    }

    else {
      val sb = new StringBuilder
      sb ++= "Invalid UTF-16 sequence at index %d: ".format(startOffset)

      sb ++= (
        for (i <- startOffset to List(startOffset + 2, code.length).min) yield
        {
          "0x%04x".format(code(i))
        }
      ).mkString(", ")

      if (code.length > startOffset + 2)
        sb ++= "..."

      throw new IllegalArgumentException(sb.result)
    }
  }

  def encodeToUtf8Array(cp: Int): Array[Byte] = {
    val builder = new ArrayBuilder.ofByte
    encodeToUtf8(cp, builder)
    builder.result
  }

  def encodeToUtf8[To[Utf8CodeUnit]](cp: Int, builder: Builder[Byte,To[Byte]]) {

    lazy val exception = new IllegalArgumentException("Invalid UTF-32 codepoint: 0x%08x".format(cp))

    if (cp <= 0x7f)
      builder += cp.asInstanceOf[Byte]    // 7 bits

    else if (cp <= 0x7ff) { // 6+5 = 11 bits (counting from right)
      val b1 = (cp >> 6         | 0xC0).asInstanceOf[Byte]
      val b2 = (cp >> 0  & 0x3f | 0x80).asInstanceOf[Byte]
      builder += (b1, b2)
    }

    else if (cp <= 0xd7ff) {
      val b1 = (cp >> 12        | 0xE0).asInstanceOf[Byte]
      val b2 = (cp >> 6  & 0x3f | 0x80).asInstanceOf[Byte]
      val b3 = (cp >> 0  & 0x3f | 0x80).asInstanceOf[Byte]
      builder += (b1, b2, b3)
    }

    else if (cp <= 0xdfff)
      throw exception

    else if (cp <= 0xfffd) {
      val b1 = (cp >> 12        | 0xE0).asInstanceOf[Byte]
      val b2 = (cp >> 6  & 0x3f | 0x80).asInstanceOf[Byte]
      val b3 = (cp >> 0  & 0x3f | 0x80).asInstanceOf[Byte]
      builder += (b1, b2, b3)
    }

    else if (cp <= 0xffff)
      throw exception

    else if (cp <= 0x10ffff) {
      val b1 = (cp >> 18 & 0x07 | 0xF0).asInstanceOf[Byte]
      val b2 = (cp >> 12 & 0x3f | 0x80).asInstanceOf[Byte]
      val b3 = (cp >> 6  & 0x3f | 0x80).asInstanceOf[Byte]
      val b4 = (cp >> 0  & 0x3f | 0x80).asInstanceOf[Byte]
      builder += (b1, b2, b3, b4)
    }

    else throw exception
  }

  def encodeToUtf16Array(cp: Int): Array[Char] = {
    val builder: ArrayBuilder[Char] = new ArrayBuilder.ofChar
    encodeToUtf16(cp, builder)
    builder.result
  }

  def encodeToUtf16[To[Char]](cp: Int, builder: Builder[Char,To[Char]]) {

    lazy val exception = new IllegalArgumentException("Invalid UTF-32 codepoint: 0x%08x".format(cp))

    if (cp <= 0xd7ff)
      builder += cp.asInstanceOf[Char]

    else if (cp <= 0xdfff)
      throw exception

    else if (cp <= 0xfffd)
      builder += cp.asInstanceOf[Char]

    else if (cp <= 0xffff)
      throw exception

    else if (cp <= 0x10FFFF) {
      val shifted = cp - 0x10000
      val s1 = ((shifted >> 10  & 0x3ff) | 0xD800).asInstanceOf[Char]
      val s2 = ((shifted >> 0   & 0x3ff) | 0xDC00).asInstanceOf[Char]
      builder += (s1,s2)
    }

    else
      throw exception
  }
}
