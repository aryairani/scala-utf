/**
 * These classes represent individual Unicode characters.
 */
sealed abstract class CodePoint {
  def boxed: CodePoint.Utf32
  def asInt: Int
  def toString: java.lang.String
}
object CodePoint {
  case class Utf8(private val underlying: Array[Byte]) extends CodePoint { /* ... */ }
  case class Utf16(private val underlying: Array[Char]) extends CodePoint { /* ... */ } 
  case class Utf32(private val underlying: Int) extends CodePoint { /* ... */ }
}

/**
 * These classes represent Unicode strings, and conversions between encodings.
 */
abstract class UtfString[U] extends Seq[CodePoint.Utf32] {
  val underlying: Array[U] /* Backed by Arrays of values, not refs */
  def length: Int
  def toUtf32String: Utf32String
  def toUtf16String: Utf16String
  def toUtf8String: Utf8String
  // ...
}
/** Utf32 supports O(1) lookup **/
class Utf32String extends UtfString[Int] with IndexedSeqOptimized[/*...*/] { /* ... */ }

/** Utf8 and Utf16 support O(n) lookup **/
class Utf16String extends UtfString[Char] { /* ... */ }
class Utf8String extends UtfString[Byte] { /* ... */ }

/**
 * Here is a little test.
 */
import net.arya.utf.Implicits._
object Test1 extends Application {
  val singles = "«küßî»"
  val doubles = "“ЌύБЇ”"

  val doubles8: Utf8String = doubles // UTF-8 uses 14 Bytes
  val doubles16: Utf16String = doubles // UTF-16 uses 6 Chars = 12 bytes
  val foo: String = doubles8.mkString(":") // .mkString is defined on Seq...
}


Join the chat at https://gitter.im/refried/scala-utf
