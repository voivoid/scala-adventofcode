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

    adventOfCode.utils.ocr.decodeChars(pixels, width)
  }

  def decodePixels(input: Input, width: Int, height: Int): Seq[Char] = {
    val layers = getLayers(input, width, height).toList

    layers.transpose.map(calcPixelColor)
  }

  private def getLayers(input: Input, width: Int, height: Int) = {
    val digitsPerLayer = width * height

    input.iterator.grouped(digitsPerLayer)
  }

  private def calcPixelColor(layers: Seq[Char]): Char = {
    val TransparentPixel = '2'
    layers.find(_ != TransparentPixel).getOrElse(sys.error("all pixels are transparent"))
  }

}
