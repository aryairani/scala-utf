package net.arya.utf

/**
 * Created by IntelliJ IDEA.
 * User: arya
 * Date: 12/26/10
 * Time: 5:44 PM
 */

import collection.IndexedSeqOptimized
import collection.mutable.ArrayBuilder

/**
 * Superclass of UtfXString
 */
abstract class UtfString[U] extends Seq[CodePoint.Utf32] {

  val underlying: Array[U]

  def length: Int

  def toUtf32String: Utf32String
  def toUtf16String: Utf16String
  def toUtf8String: Utf8String

  override def toString = new java.lang.String(toUtf16String.underlying)
}

object UtfString {
  def apply(underlying: Array[Byte]) = new Utf8String(underlying)
  def apply(underlying: Array[Char]) = new Utf16String(underlying)
  def apply(underlying: Array[Int]) = new Utf32String(underlying)
  def apply(underlying: CodePoint.Utf32) = new Utf32String(Array(underlying.asInt))
  def apply(underlying: Int) = new Utf32String(Array(underlying))
}


case class UtfDecodeResult(asInt: Int, codeUnitsConsumed: Int) {
  def this(boxed: CodePoint.Utf32, codeUnitsConsumed: Int) =
    this(boxed.asInt,  codeUnitsConsumed)
  def boxed = CodePoint.Utf32(asInt)
}


/**
 * Trait for dealing with multi-unit codepoint encodings
 */
trait EncodedUtfString[U]
  extends UtfString[U]
{

  def decodeAt(index: Int): UtfDecodeResult

  lazy val length = {
    var count = 0;
    val it = this.iterator;
    while (it.hasNext) { it.next; count += 1 }
    count
  }

  override def apply(idx: Int): CodePoint.Utf32 = {
    val it = this.iterator
    var count = 0;
    while (count < idx) {
      if (iterator.hasNext) iterator.next
      else throw new IndexOutOfBoundsException
      count += 1
    }
    if (iterator.hasNext) iterator.next
    else throw new IndexOutOfBoundsException
  }

  override def iterator: Iterator[CodePoint.Utf32] =
    new Iterator[CodePoint.Utf32] {
      var index = 0;

      override def hasNext: Boolean =
        index < underlying.length

      override def next: CodePoint.Utf32 = {
        val result = decodeAt(index)

        index += result.codeUnitsConsumed

        result.boxed
      }
    }

  override def toUtf32String = {
    val builder = new ArrayBuilder.ofInt
    var unitIndex = 0;
    while (unitIndex < underlying.length) {
      val result = decodeAt(unitIndex)
      builder += result.asInt
      unitIndex += result.codeUnitsConsumed
    }
    new Utf32String(builder.result)
  }

  def toUtf16String: Utf16String = this.toUtf32String.toUtf16String
  def toUtf8String: Utf8String = this.toUtf32String.toUtf8String

}

/**
 * Utf8String
 */
class Utf8String(override val underlying: Array[Byte])
  extends EncodedUtfString[Byte]
{
  def decodeAt(startOffset: Int): UtfDecodeResult = Util.decodeAt(underlying, startOffset)
}


/**
 * Utf16String
 */
class Utf16String(override val underlying: Array[Char])
  extends EncodedUtfString[Char]
{
  def decodeAt(arrayIndex: Int): UtfDecodeResult = Util.decodeAt(underlying, arrayIndex)
}


/**
 * Utf32String
 */
class Utf32String(val underlying: Array[Int])
  extends UtfString[Int]
  with IndexedSeqOptimized[CodePoint.Utf32, Seq[CodePoint.Utf32]]
{
  override def apply(idx: Int) = CodePoint.Utf32(underlying(idx))
  override def length = underlying.length

  def toUtf32String = this

  def toUtf16String = {
    val builder = new ArrayBuilder.ofChar
    for (cp <- underlying)
      Util.encodeToUtf16(cp, builder)
    new Utf16String(builder.result)
  }

  def toUtf8String = {
    val builder = new ArrayBuilder.ofByte
    for (cp <- underlying)
      Util.encodeToUtf8(cp, builder)
    new Utf8String(builder.result)
  }
}
