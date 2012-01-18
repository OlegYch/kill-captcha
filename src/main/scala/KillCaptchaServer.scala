import KillCaptcha.CaptchaResult
import util.Properties

/**
 */

object KillCaptchaServer extends App {

  import unfiltered.request._
  import unfiltered.response._

  val Solver = unfiltered.filter.Planify {
    case req@GET(_) =>
      println("GET ")
      view()
    case req@POST(Path("/solve")) =>
      println("POST /solve")
      val MultiPart(form) = req
      displayResult(KillCaptcha.recaptcha(MultiPartParams.Memory(form).files("captcha")(0).bytes))
  }

  def view() = {
    Html(<html>Enter file:
      <form action='/solve' method='post' enctype='multipart/form-data'>
          <input type='file' name='captcha'/>
          <input type='submit'/>
      </form>
    </html>)
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

  unfiltered.jetty.Http(Properties.envOrElse("PORT", "8151").toInt)
    .filter(Solver)
    .run {s =>
    try unfiltered.util.Browser.open("http://127.0.0.1:%d/".format(s.port)) catch {case e => println(e)}
  }
}
