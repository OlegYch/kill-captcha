import collection.Seq
import com.google.common.io.{Files, ByteStreams}
import java.io.File
import javax.imageio.ImageIO
import org.encog.neural.networks.BasicNetwork
import org.encog.util.obj.SerializeObject

/**
 */

object KillCaptcha {
  val networkFile = new File("network")
  def network = SerializeObject.load(networkFile).asInstanceOf[BasicNetwork]

  case class CaptchaResult(result: String, confidence: Double)

  def recaptcha(bytes: Array[Byte]): CaptchaResult = {
    val captcha = ImageIO.read(ByteStreams.newInputStreamSupplier(bytes).getInput)
    println("size = " + bytes.size)
    val recognizedNumbers: Seq[(Double, Int)] = SplitImage.splitCaptcha(new ImageSplitter(captcha)).map(c =>
      TrainNetwork.compute(TrainNetwork.readImageInput(c), network))
    CaptchaResult(recognizedNumbers.map(_._2).mkString, recognizedNumbers.map(_._1).product)
  }

  def main(args: Array[String]) {
    println(recaptcha(Files.toByteArray(new File(args(0)))))
  }
}
