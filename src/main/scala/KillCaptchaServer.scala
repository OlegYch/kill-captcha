import collection.immutable.IndexedSeq
import com.google.common.io.Files
import java.io.File
import java.lang.{Thread, String}
import java.util.UUID
import KillCaptcha.CaptchaResult
import util.Properties

/**
 */

object KillCaptchaServer extends App {

  import unfiltered.request._
  import unfiltered.response._

  val Solver = unfiltered.filter.Planify {
    case req@POST(Path("/solve")) =>
      println("POST solve")
      val MultiPart(form) = req
      displayResult(KillCaptcha.recaptcha(MultiPartParams.Memory(form).files("captcha")(0).bytes))
    case req@POST(Path(Seg("train" :: uid :: Nil))) =>
      println("POST train/" + uid)
      val Params(form) = req
      val answer = form("answer")(0)
      println("answer " + answer)
      CaptchasToSolve.solve(uid, answer)
      Redirect("")
    case req@GET(Path("/train")) =>
      println("GET train")
      val uid = UUID.randomUUID().toString
      displayTrain(uid, CaptchasToSolve.get(uid))
    case req@GET(Path("/network")) =>
      println("GET network")
      ResponseBytes(Files.toByteArray(NetworkConstants.networkFile))
    case req@GET(_) =>
      println("GET ")
      view()
  }

  def view() = {
    Html(<html>Enter file:
      <form action='/solve' method='post' enctype='multipart/form-data'>
          <input type='file' name='captcha'/>
          <input type='submit' value='Solve'/>
      </form>
      <a href="/train">Train</a>
      <a href="/network">Get Network</a>
    </html>)
  }

  def displayTrain(uid: String, files: IndexedSeq[File]) = {
    import org.apache.commons.codec.binary.Base64
    val action = "/train/" + uid
    val img: String = "data:image/png;base64," + new Base64().encodeToString(Files.toByteArray(files(0)))
    Html(
      <html>
        <form action={action} method='post'>
            <img src={img}/>
            <input type='text' name='answer'/>
            <input type='submit' value='Answer'/>
        </form>
      </html>
    )
  }

  def displayResult(result: CaptchaResult) = {
    Html(
      <html>
        <body>
          The answer is:
          {result.result}
          Confidence is:
          {result.confidence}
        </body>
      </html>
    )
  }

  new Thread(new Runnable {
    def run() {
      val network = KillCaptcha.network
      while (true) {
        network.trainNetwork(network.set, iterations = 1)
      }
    }
  }) {
    setDaemon(true)
    start()
  }
  unfiltered.jetty.Http(Properties.envOrElse("PORT", "8151").toInt)
    .filter(Solver)
    .run {s =>
    try unfiltered.util.Browser.open("http://127.0.0.1:%d/".format(s.port)) catch {case e => println(e)}
  }
}
