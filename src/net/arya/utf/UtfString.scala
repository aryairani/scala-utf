package net.arya.utf

/**
 * Created by IntelliJ IDEA.
 * User: arya
 * Date: 12/26/10
 * Time: 5:44 PM
 */

import Types._

import collection.IndexedSeqOptimized
import collection.mutable.ArrayBuilder

case class UtfDecodeResult(codePoint: Utf32CodePoint, codeUnitsConsumed: Int)

abstract class UtfString[U] extends Seq[Utf32CodePoint] {

  val underlying: Array[U]

  def length: Int

  def toUtf32String: Utf32String
  def toUtf16String: Utf16String
  def toUtf8String: Utf8String
  override def toString = new String(toUtf16String.underlying)
}

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

  override def apply(idx: Int): Utf32CodePoint = {
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

  override def iterator: Iterator[Utf32CodePoint] = new Iterator[Utf32CodePoint] {
    var index = 0;

    override def hasNext: Boolean = index >= underlying.length

    override def next: Utf32CodePoint = {
      val result = decodeAt(index)

      index += result.codeUnitsConsumed

      result.codePoint
    }
  }

  override def toUtf32String = {
    val builder = new ArrayBuilder.ofInt
    var unitIndex = 0;
    while (unitIndex < underlying.length) {
      val result = decodeAt(unitIndex)
      builder += result.codePoint
      unitIndex += result.codeUnitsConsumed
    }
    new Utf32String(builder.result)
  }

  def toUtf16String: Utf16String = this.toUtf32String.toUtf16String
  def toUtf8String: Utf8String = this.toUtf32String.toUtf8String

}

class Utf8String(override val underlying: Array[Utf8CodeUnit])
  extends EncodedUtfString[Utf8CodeUnit]
{
  def decodeAt(startOffset: Int): UtfDecodeResult = Unicode.decodeAt(underlying, startOffset)
}

class Utf16String(override val underlying: Array[Utf16CodeUnit])
  extends EncodedUtfString[Utf16CodeUnit]
{
  def decodeAt(arrayIndex: Int): UtfDecodeResult = Unicode.decodeAt(underlying, arrayIndex)
}

class Utf32String(val underlying: Array[Utf32CodeUnit])
  extends UtfString[Utf32CodeUnit]
  with IndexedSeqOptimized[Utf32CodePoint,Seq[Utf32CodePoint]]
{
  override def apply(idx: Int) = underlying(idx)
  override def length = underlying.length

  def toUtf32String = this

  def toUtf16String = {
    val builder = new ArrayBuilder.ofChar
    for (cp <- underlying)
      Unicode.encodeToUtf16(cp, builder)
    new Utf16String(builder.result)
  }

  def toUtf8String = {
    val builder = new ArrayBuilder.ofByte
    for (cp <- underlying)
      Unicode.encodeToUtf8(cp, builder)
    new Utf8String(builder.result)
  }
}
