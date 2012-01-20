import java.io.File
import java.util.concurrent.atomic.AtomicInteger
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

object SplitImage extends App {

  import NetworkConstants._

  implicit def withSafeFiles(f: File) = new {
    def listFilesSafe: Seq[File] = for {
      children <- Option(f.listFiles()).toSeq
      child <- children.sortBy {n =>
        val name = n.getName
        val numbers = try name.filter(_.isDigit).toInt catch {case e: NumberFormatException => 0}
        (numbers, name)
      }
    } yield {
      child
    }
  }

  def splitCaptcha(splitImage: ImageSplitter): Seq[BufferedImage] = {
    val numbers = splitImage.splitImages(splitImage.evenSplits).map {image =>
      val splitter = new ImageSplitter(ImageUtils.rotateImage(image)) {
        override val numberOfSplits = 1
      }
      ImageUtils.scaleImage(
        ImageUtils.rotateImage(splitter.splitImages(splitter.evenSplits)(0), angle = -math.Pi / 2)
        , width = imageDimensions, height = imageDimensions)
    }
    numbers
  }

  val counter = new AtomicInteger(0)
  new File(dataDir).listFilesSafe.par.filter(_.getName.length == 4).map {f =>
    val captcha = f.getName
    f.listFilesSafe.par.filterNot(_.getName.contains("_")).map {c =>
      println("processing " + c)
      try {
        val splitImage = new ImageSplitter(ImageUtils.convertToBW(ImageUtils.readImage(c)))
        //        println(splitImage.lines.map(_.sum))
        //        println(splitImage.diffs)
        //        println(splitImage.euristicSplits)
        //        ImageIO.write(splitImage.writeSplits(splitImage.evenSplits), "png",
        //          new File(c.getAbsolutePath.replace(".png", "_.png")))
        val numbers: Seq[BufferedImage] = splitCaptcha(splitImage)
        numbers.zip(captcha).foreach {
          case (image, char) =>
            val charDir = new File(f.getParentFile, char.toString)
            charDir.mkdirs()
            val charFile = new File(charDir, counter.incrementAndGet() + ".png")
            ImageIO.write(image, "png", charFile)
        }
        numbers
      } catch {
        case e => e.printStackTrace()
      }
    }
  }
}
