package net.arya.utf

/**
 * Created by IntelliJ IDEA.
 * User: arya
 * Date: 12/27/10
 * Time: 10:12 PM
 */

object Types {
  type Utf8CodeUnit = Byte
  type Utf16CodeUnit = Char
  type Utf32CodeUnit = Int
  case class Utf8CodePoint(private val underlying: Array[Utf8CodeUnit])
  case class Utf16CodePoint(private val underlying: Array[Utf16CodeUnit])
  type Utf32CodePoint = Utf32CodeUnit
  case class RichUtf32CodePoint(private val underlying: Utf32CodePoint)
}