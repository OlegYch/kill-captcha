import collection.Seq
import com.google.common.io.{Files, ByteStreams}
import java.io.File

/**
 */

object KillCaptcha {
  def network = TrainNetwork.load

  case class CaptchaResult(result: String, confidence: Double)

  def recaptcha(bytes: Array[Byte]): CaptchaResult = {
    val recognizedNumbers: Seq[(Double, Int)] = SplitImage
      .splitCaptcha(new ImageSplitter(ByteStreams.newInputStreamSupplier(bytes).getInput)).map(c =>
      network.compute(network.readImageInput(c)))
    CaptchaResult(recognizedNumbers.map(_._2).mkString, recognizedNumbers.map(_._1).product)
  }

  def main(args: Array[String]) {
    println(recaptcha(Files.toByteArray(new File(args(0)))))
  }
}
