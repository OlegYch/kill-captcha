import com.google.common.io.Files
import java.io.{ByteArrayOutputStream, FileOutputStream, File}
import unfiltered.util.Browser

class GetCaptcha {

  import dispatch._

  val u = url("http://egrul.nalog.ru/i/def.png")
  var cookie: Option[String] = None

  def r(out: java.io.OutputStream) = u.<:<(cookie.map(c => "cookie" -> c).toMap).>:+ {(h, r) =>
    h("set-cookie").foreach(c => cookie = Some(c))
    r.>>>(out)
  }

  val h = new Http
}

object CaptchasToSolve {
  def resultDir(uid: String) = new File(NetworkConstants.dataDir, uid)

  def get(uid: String) = {
    val c = new GetCaptcha
    import c._
    for (i <- 1 to 10) yield {
      val out = new ByteArrayOutputStream()
      h(r(out))
      val bytes = out.toByteArray
      resultDir(uid).mkdirs()
      val file = new File(resultDir(uid), i + ".png")
      Files.write(bytes, file)
      file
    }
  }

  def solve(uid: String, answer: String) {
    val result = resultDir(uid)
    Files.move(result, new File(NetworkConstants.dataDir, answer))
    SplitImage.main(null)
  }
}

object GetCaptcha extends GetCaptcha with App {

  import NetworkConstants._

  val files = for (i <- 1 to 1000) yield {
    val file: File = new File("captcha" + i + ".png")
    val out: FileOutputStream = new FileOutputStream(file)
    h(r(out))
    out.close()
    file
  }
  Browser.open(files.head.toURI.toString)
  val answer = readLine()
  files.foreach {f =>
    val answerDataDir: File = new File(f.getParentFile, dataDir + answer)
    answerDataDir.mkdirs
    Files.move(f, new File(answerDataDir, f.getName))
  }
}

object TestCaptchas extends App {
  for (i <- 1 to 20) {
    new GetCaptcha {
      val out = new ByteArrayOutputStream()
      h(r(out))
      val bytes = out.toByteArray
      val answer = KillCaptcha.recaptcha(bytes).result
      Files.write(bytes, new File(answer + ".png"))
    }
  }
}
