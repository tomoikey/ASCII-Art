import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import java.awt.Image.SCALE_AREA_AVERAGING
import javax.imageio.ImageIO

@main def main(args: String*): Unit = println(
  ImageIO
    .read(new File(args.head))
    .rescale(args(1).toDouble)
    .toGrayScale
    .asciiArt
)

extension (image: BufferedImage)
  def rescale(scale: Double): BufferedImage = {
    val (originalWidth, originalHeight) =
      (image.getWidth.toDouble, image.getHeight.toDouble)
    val (widthScale, heightScale) =
      if (originalWidth > originalHeight) (1d, originalHeight / originalWidth)
      else (originalWidth / originalHeight, 1d)
    val (newWidth, newHeight) =
      ((scale * widthScale).toInt, (scale * heightScale).toInt)
    val newImage = new BufferedImage(newWidth, newHeight, image.getType)
    val scaled =
      image.getScaledInstance(newWidth, newHeight, SCALE_AREA_AVERAGING)
    newImage.getGraphics.drawImage(scaled, 0, 0, newWidth, newHeight, null)
    newImage
  }

  def toGrayScale: BufferedImage = {
    val grayImage = image
    for (i <- 0 until image.getHeight) for (j <- 0 until image.getWidth) {
      val rgb = grayImage.getRGB(i, j)
      val (a, r, g, b) =
        ((rgb >> 24) & 0xff, (rgb >> 16) & 0xff, (rgb >> 8) & 0xff, rgb & 0xff)
      val gamma =
        (0.299 * r + 0.587 * g + 0.114 * b).toInt // ITU-R Rec BT.601規格
      grayImage.setRGB(i, j, (a << 24) + (gamma << 16) + (gamma << 8) + gamma)
    }
    grayImage
  }

  def asciiArt: String = (for (i <- 0 until image.getHeight)
    yield (for (j <- 0 until image.getWidth)
      yield AsciiArtConverter(new Color(image.getRGB(j, i))).toChar)
      .mkString(" ")).mkString("\n")

final case class AsciiArtConverter(color: Color):
  assert(color.getRed == color.getGreen && color.getGreen == color.getBlue)
  private val charConversion: PartialFunction[Int, Char] = {
    case x if x < 32  => ' '
    case x if x < 64  => '.'
    case x if x < 96  => ','
    case x if x < 128 => '-'
    case x if x < 160 => '='
    case x if x < 192 => '1'
    case x if x < 224 => '@'
    case _            => 'W'
  }
  def toChar: Char = charConversion(color.getRed)
