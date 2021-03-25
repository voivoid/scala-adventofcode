package adventOfCode.problems
package year2019

object problem08 extends baseProblem {

  override def solve1(input: Input): Int = solve1(input, 25, 6)

  def solve1(input: Input, width: Int, height: Int): Int = {
    val layers = getLayers(input, width, height)
    val layerWithMinZeroes = layers.minBy(_.count(_ == '0'))

    layerWithMinZeroes.count(_ == '1') * layerWithMinZeroes.count(_ == '2')
  }

  override def solve2(input: Input): String = {
    val width = 25
    val height = 6
    val pixels = decodePixels(input, width, height)

    decodeChars(pixels, width)
  }

  def decodePixels(input: Input, width: Int, height: Int): Seq[Char] = {
    val layers = getLayers(input, width, height).toList

    layers.transpose.map(calcPixelColor)
  }

  private def getLayers(input: Input, width: Int, height: Int) = {
    val digitsPerLayer = width * height

    input.iterator.grouped(digitsPerLayer)
  }

  private def decodeChars(pixels: Pixels, width: Int): String = {
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

  private def calcPixelColor(layers: Seq[Char]): Char = {
    val TransparentPixel = '2'
    layers.find(_ != TransparentPixel).getOrElse(sys.error("all pixels are transparent"))
  }

  private type Pixels = Seq[Char]

  private def pixelsToCharMap: Map[Seq[Char], Char] = {
    val * = '1'
    val - = '0'

    // format: off

    val chars: Seq[(Seq[Char], Char)] = Seq(

      Seq( -, *, *, -, -,
           *, -, -, *, -,
           *, -, -, *, -,
           *, *, *, *, -,
           *, -, -, *, -,
           *, -, -, *, - ) -> 'A',

      Seq( *, *, *, -, -,
           *, -, -, *, -,
           *, *, *, -, -,
           *, -, -, *, -,
           *, -, -, *, -,
           *, *, *, -, - ) -> 'B',

      Seq( -, *, *, -, -,
           *, -, -, *, -,
           *, -, -, -, -,
           *, -, -, -, -,
           *, -, -, *, -,
           -, *, *, -, -) -> 'C',

      Seq( *, *, *, *, -,
           *, -, -, -, -,
           *, *, *, -, -,
           *, -, -, -, -,
           *, -, -, -, -,
           *, *, *, *, -) -> 'E',

      Seq( *, *, *, *, -,
           *, -, -, -, -,
           *, *, *, -, -,
           *, -, -, -, -,
           *, -, -, -, -,
           *, -, -, -, - ) -> 'F',

      Seq( -, *, *, -, -,
           *, -, -, *, -,
           *, -, -, -, -,
           *, -, *, *, -,
           *, -, -, *, -,
           -, *, *, *, -) -> 'G',

      Seq( *, -, -, *, -,
           *, -, -, *, -,
           *, *, *, *, -,
           *, -, -, *, -,
           *, -, -, *, -,
           *, -, -, *, - ) -> 'H',

      Seq( -, -, *, *, -,
           -, -, -, *, -,
           -, -, -, *, -,
           -, -, -, *, -,
           *, -, -, *, -,
           -, *, *, -, - ) -> 'J',

      Seq( *, -, -, *, -,
           *, -, *, -, -,
           *, *, -, -, -,
           *, -, *, -, -,
           *, -, *, -, -,
           *, -, -, *, -) -> 'K',

      Seq( *, -, -, -, -,
           *, -, -, -, -,
           *, -, -, -, -,
           *, -, -, -, -,
           *, -, -, -, -,
           *, *, *, *, - ) -> 'L',

      Seq( *, *, *, -, -,
           *, -, -, *, -,
           *, -, -, *, -,
           *, *, *, -, -,
           *, -, -, -, -,
           *, -, -, -, -) -> 'P',

      Seq( *, *, *, -, -,
           *, -, -, *, -,
           *, -, -, *, -,
           *, *, *, -, -,
           *, -, *, -, -,
           *, -, -, *, -) -> 'R',

      Seq( *, -, -, *, -,
           *, -, -, *, -,
           *, -, -, *, -,
           *, -, -, *, -,
           *, -, -, *, -,
           -, *, *, -, -) -> 'U',

      Seq( *, -, -, -, *,
           *, -, -, -, *,
           -, *, -, *, -,
           -, -, *, -, -,
           -, -, *, -, -,
           -, -, *, -, - ) -> 'Y',

      Seq( *, *, *, *, -,
           -, -, -, *, -,
           -, -, *, -, -,
           -, *, -, -, -,
           *, -, -, -, -,
           *, *, *, *, - ) -> 'Z'
    )

    // format: on

    chars.toMap
  }

}
