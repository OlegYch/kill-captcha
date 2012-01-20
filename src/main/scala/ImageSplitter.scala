import java.awt.image.BufferedImage
import java.io.{InputStream, File}
import javax.imageio.ImageIO

class ImageSplitter(val image: BufferedImage) {
  val numberOfSplits = 4
  val lineWidth = 1

  val lines = {
    for (x <- 0 until (image.getWidth - 1) by lineWidth) yield {
      image.getRGB(x, 0, lineWidth, image.getHeight, null, 0, lineWidth)
    }
  }

  case class Line(index: Int, diff: Int)

  val diffs = lines.map(_.sum).sliding(3).map(s => (s(1) * 2 - s(0) - s(2)).abs).zipWithIndex
    .map(p => Line(p._2, p._1)).toIndexedSeq
  lazy val euristicSplits = diffs.view.groupBy(start => start.index / 7).toSeq.map(_._2.maxBy(-_.diff)).
    sortBy(-_.diff).take(numberOfSplits).sortBy(_.index)

  lazy val startOfNumbers = diffs.find(_.diff > 0).get
  lazy val endOfNumbers = diffs.view.reverse.find(_.diff > 0).get

  lazy val splitWidth = (endOfNumbers.index - startOfNumbers.index).toDouble / numberOfSplits
  lazy val evenSplits = (startOfNumbers.index.toDouble until endOfNumbers.index by splitWidth).map(_.toInt)

  def writeSplits(splits: Seq[Int] = euristicSplits.map(_.index)): BufferedImage = {
    val image = ImageUtils.deepCopy(this.image)
    val graphics = image.createGraphics()
    graphics.setColor(java.awt.Color.BLACK)
    splits.foreach(l => graphics.drawRect(l * lineWidth, 0, lineWidth, image.getHeight))
    graphics.dispose()
    image
  }

  def splitImages(splits: Seq[Int]): Seq[BufferedImage] = {
    splits.map {l =>
      val x = l * lineWidth
      ImageUtils.deepCopy(image.getSubimage(x, 0, (image.getWidth - x).min(splitWidth.floor.toInt),
        image.getHeight))
    }
  }
}

object ImageUtils {

  import java.awt._

  def withGraphics[T](image: BufferedImage)(f: Graphics2D => T): T = {
    val g = image.createGraphics()
    val result = f(g)
    g.dispose()
    result
  }

  def deepCopy(originalImage: BufferedImage): BufferedImage = {
    val image = new BufferedImage(originalImage.getWidth, originalImage.getHeight,
      BufferedImage.TYPE_INT_RGB)
    withGraphics(image) {g => g.drawImage(originalImage, 0, 0, null)}
    image
  }

  def rotateImage(image: BufferedImage, angle: Double = math.Pi / 2): BufferedImage = {
    val sin = math.abs(math.sin(angle))
    val cos = math.abs(math.cos(angle))
    val w = image.getWidth
    val h = image.getHeight
    val neww = math.floor(w * cos + h * sin).toInt
    val newh = math.floor(h * cos + w * sin).toInt
    val result = new BufferedImage(neww, newh, Transparency.OPAQUE)
    withGraphics(result) {g =>
      g.translate((neww - w) / 2, (newh - h) / 2)
      g.rotate(angle, w / 2, h / 2)
      g.drawRenderedImage(image, null)
    }
    result
  }

  def scaleImage(image: BufferedImage, width: Int, height: Int): BufferedImage = {
    val w = image.getWidth
    val h = image.getHeight
    val neww = width
    val newh = height
    val result: BufferedImage = new BufferedImage(neww, newh, Transparency.OPAQUE)
    withGraphics(result) {g =>
      g.scale(neww.toDouble / w, newh.toDouble / h)
      g.drawRenderedImage(image, null);
    }
    result
  }

  def convertToBW(image: BufferedImage) = {
    val blackAndWhiteImage = new BufferedImage(image.getWidth(null), image.getHeight(null),
      BufferedImage.TYPE_BYTE_BINARY);
    withGraphics(blackAndWhiteImage)(_.drawImage(image, 0, 0, null))
    blackAndWhiteImage
  }

  def readImage(f:InputStream) = ImageIO.read(f)

  def readImage(f:File) = ImageIO.read(f)
}
