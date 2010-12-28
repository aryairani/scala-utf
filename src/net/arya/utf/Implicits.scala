package net.arya.utf

/**
 * Created by IntelliJ IDEA.
 * User: arya
 * Date: 12/27/10
 * Time: 11:24 PM
 * To change this template use File | Settings | File Templates.
 */

object Implicits {
  implicit def stringToUtf8String(s: java.lang.String) = new Utf16String(s.toCharArray).toUtf8String
  implicit def stringToUtf16String(s: java.lang.String) = new Utf16String(s.toCharArray)
  implicit def stringToUtf32String(s: java.lang.String) = new Utf16String(s.toCharArray).toUtf32String
  implicit def richUtf32cp(cp: Int) = new CodePoint.Utf32(cp)
}

