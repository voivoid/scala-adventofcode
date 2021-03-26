package adventOfCode.utils

package object ocr {

  def decodeChars(pixels: Seq[Char], width: Int): String = {
    val charWidth = 5
    val charsNum = width / charWidth

    val indexedPixels = pixels.zipWithIndex

    val charsPixels = (0 until charsNum).map(charN => {
      val fromPixelIndex = charN * charWidth
      val toPixelindex = fromPixelIndex + charWidth
      val pixelRange = fromPixelIndex until toPixelindex

      indexedPixels.withFilter { case (_, pixelIndex) => pixelRange.contains(pixelIndex % width) }.map(_._1)
    })

    charsPixels.map(pixelsToCharMap).mkString
  }

  def pixelsToCharMap: Map[Seq[Char], Char] = {
    val █ = '1'
    val - = '0'

    // format: off

    val chars: Seq[(Seq[Char], Char)] = Seq(

      Seq( -, █, █, -, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, █, █, █, -,
           █, -, -, █, -,
           █, -, -, █, - ) -> 'A',

      Seq( █, █, █, -, -,
           █, -, -, █, -,
           █, █, █, -, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, █, █, -, - ) -> 'B',

      Seq( -, █, █, -, -,
           █, -, -, █, -,
           █, -, -, -, -,
           █, -, -, -, -,
           █, -, -, █, -,
           -, █, █, -, -) -> 'C',

      Seq( █, █, █, █, -,
           █, -, -, -, -,
           █, █, █, -, -,
           █, -, -, -, -,
           █, -, -, -, -,
           █, █, █, █, -) -> 'E',

      Seq( █, █, █, █, -,
           █, -, -, -, -,
           █, █, █, -, -,
           █, -, -, -, -,
           █, -, -, -, -,
           █, -, -, -, - ) -> 'F',

      Seq( -, █, █, -, -,
           █, -, -, █, -,
           █, -, -, -, -,
           █, -, █, █, -,
           █, -, -, █, -,
           -, █, █, █, -) -> 'G',

      Seq( █, -, -, █, -,
           █, -, -, █, -,
           █, █, █, █, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, -, -, █, - ) -> 'H',

      Seq( -, █, █, █, -,
           -, -, █, -, -,
           -, -, █, -, -,
           -, -, █, -, -,
           -, -, █, -, -,
           -, █, █, █, - ) -> 'I',

      Seq( -, -, █, █, -,
           -, -, -, █, -,
           -, -, -, █, -,
           -, -, -, █, -,
           █, -, -, █, -,
           -, █, █, -, - ) -> 'J',

      Seq( █, -, -, █, -,
           █, -, █, -, -,
           █, █, -, -, -,
           █, -, █, -, -,
           █, -, █, -, -,
           █, -, -, █, -) -> 'K',

      Seq( █, -, -, -, -,
           █, -, -, -, -,
           █, -, -, -, -,
           █, -, -, -, -,
           █, -, -, -, -,
           █, █, █, █, - ) -> 'L',

      Seq( -, █, █, -, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, -, -, █, -,
           -, █, █, -, -) -> 'O',

      Seq( █, █, █, -, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, █, █, -, -,
           █, -, -, -, -,
           █, -, -, -, -) -> 'P',

      Seq( █, █, █, -, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, █, █, -, -,
           █, -, █, -, -,
           █, -, -, █, -) -> 'R',

      Seq( █, -, -, █, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, -, -, █, -,
           █, -, -, █, -,
           -, █, █, -, -) -> 'U',

      Seq( █, -, -, -, █,
           █, -, -, -, █,
           -, █, -, █, -,
           -, -, █, -, -,
           -, -, █, -, -,
           -, -, █, -, - ) -> 'Y',

      Seq( █, █, █, █, -,
           -, -, -, █, -,
           -, -, █, -, -,
           -, █, -, -, -,
           █, -, -, -, -,
           █, █, █, █, - ) -> 'Z'
    )

    // format: on

    chars.toMap
  }

}
