import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

@main def main(args: String*) = println(
  ImageIO
    .read(new File(args.head))
    .rescaleImage(args(1).toDouble)
    .imageToGrayScale
    .generateAscii
)

extension (image: BufferedImage)
  def rescaleImage(scale: Double): BufferedImage = {
    val (originalWidth, originalHeight) =
      (image.getWidth.toDouble, image.getHeight.toDouble)
    val (widthScale, heightScale) =
      if (originalWidth > originalHeight) (1d, originalHeight / originalWidth)
      else (originalWidth / originalHeight, 1d)
    val (newWight, newHeight) =
      ((scale * widthScale).toInt, (scale * heightScale).toInt)
    val newImage = new BufferedImage(newWight, newHeight, image.getType)
    val scaled = image.getScaledInstance(
      newWight,
      newHeight,
      java.awt.Image.SCALE_AREA_AVERAGING
    )
    newImage.getGraphics.drawImage(scaled, 0, 0, newWight, newHeight, null)
    newImage
  }

  def imageToGrayScale: BufferedImage = {
    val grayImage = image
    val (height, width) = (image.getHeight, image.getWidth)
    for (i <- 0 until height) for (j <- 0 until width) {
      val rgb = grayImage.getRGB(i, j)
      val (a, r, g, b) =
        ((rgb >> 24) & 0xff, (rgb >> 16) & 0xff, (rgb >> 8) & 0xff, rgb & 0xff)
      val gamma =
        (0.299 * r + 0.587 * g + 0.114 * b).toInt // ITU-R Rec BT.601規格
      grayImage.setRGB(i, j, (a << 24) + (gamma << 16) + (gamma << 8) + gamma)
    }
    grayImage
  }

  def generateAscii: String = {
    val height = image.getHeight
    val width = image.getWidth
    (for (i <- 0 until height) yield (for (j <- 0 until width) yield {
      val color = new Color(image.getRGB(j, i))
      val rgb = RGB(color.getRed, color.getGreen, color.getBlue)
      rgb.toAscii()
    }).mkString(" ")).mkString("\n")
  }

case class RGB(red: Int, green: Int, blue: Int) {
  assert(red == green && green == blue)
  private val charConversion: PartialFunction[Int, String] = {
    case x if x < 32  => " "
    case x if x < 64  => "."
    case x if x < 96  => ","
    case x if x < 128 => "-"
    case x if x < 160 => "="
    case x if x < 192 => "1"
    case x if x < 224 => "@"
    case _            => "W"
  }
  def toAscii(): String = charConversion(red)
}
