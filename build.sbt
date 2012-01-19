import com.typesafe.startscript.StartScriptPlugin

seq(StartScriptPlugin.startScriptForClassesSettings: _*)

name := "kill-captcha"

organization := "com.olegych"

{
  val dispatchVersion: String = "0.8.7"
  def dispatch(lib: String) = {
    libraryDependencies += "net.databinder" %% lib % dispatchVersion
  }
  dispatch("dispatch-http-json")
  dispatch("dispatch-http")
}

libraryDependencies += "com.google.guava" % "guava" % "11.0.1"

libraryDependencies += "org.encog" % "encog-core" % "3.0.0"

libraryDependencies += "net.databinder" %% "unfiltered-jetty" % "0.5.3"

libraryDependencies += "net.databinder" %% "unfiltered-uploads" % "0.5.3"
