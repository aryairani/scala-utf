package net.arya.utf.test

/**
 * Created by IntelliJ IDEA.
 * User: arya
 * Date: 12/27/10
 * Time: 11:37 PM
 * To change this template use File | Settings | File Templates.
 */
import net.arya.utf._
import net.arya.utf.Implicits._

object Test1 extends Application {
  val singles = "«küßî»"
  val doubles = "“ЌύБЇ”"

  val singles8: Utf8String = singles
  val doubles8: Utf8String = doubles

  val singles16: Utf16String = singles
  val doubles16: Utf16String = doubles

  val singles32: Utf32String = singles
  val doubles32: Utf32String = doubles

  val s8to16 = singles8.toUtf16String
  val d8to16 = doubles8.toUtf16String
  val s8to32 = singles8.toUtf32String
  val d8to32 = doubles8.toUtf32String

  val s16to8 = singles16.toUtf8String
  val d16to8 = doubles16.toUtf8String
  val s16to32 = singles16.toUtf32String
  val d16to32 = doubles16.toUtf32String

  val s32to8 = singles32.toUtf8String
  val d32to8 = doubles32.toUtf8String
  val s32to16 = singles32.toUtf16String
  val d32to16 = doubles32.toUtf16String

  println("singles == " + singles)
  println("doubles == " + doubles)
  println
  println("singles: Utf8String == " + singles8)
  println("doubles: Utf8String == " + doubles8)
  println
  println("singles: Utf16String == " + singles16)
  println("doubles: Utf16String == " + doubles16)
  println
  println("singles: Utf32String == " + singles32)
  println("doubles: Utf32String == " + doubles32)
  println
  println("singles8.toUtf16String == " + s8to16)
  println("doubles8.toUtf16String == " + d8to16)
  println("singles8.toUtf32String == " + s8to32)
  println("doubles8.toUtf32String == " + d8to32)
  println
  println("doubles8.mkString(\":\") == " + doubles8.mkString(":"))
  println
  println("singles16.toUtf8String == " + s16to8)
  println("doubles16.toUtf8String == " + d16to8)
  println("singles16.toUtf32String == " + s16to32)
  println("doubles16.toUtf32String == " + d16to32)
  println
  println("singles32.toUtf8String == " + s32to8)
  println("doubles32.toUtf8String == " + d32to8)
  println("singles32.toUtf16String == " + s32to16)
  println("doubles32.toUtf16String == " + d32to16)
}