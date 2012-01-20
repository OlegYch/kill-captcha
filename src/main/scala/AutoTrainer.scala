/**
 */

class AutoTrainer {
  val thread = new Thread(new Runnable {
    def run() {
      val network = KillCaptcha.network
      def test: network.TestResults = network.testNetwork(util.Random.shuffle(network.set).take(100))
      var lastTestResults = test
      while (true) {
        import dispatch._
        import dispatch.futures._
        DefaultFuture.future(new Http()(url("http://blooming-journey-5754.herokuapp.com").as_str))
        println("train error = " + network.trainNetwork(network.set))
        val testResults = test
        if (testResults.successRate > lastTestResults.successRate) {
          lastTestResults = testResults
          network.save
        }
        Thread.sleep(10000)
      }
    }
  }) {
    setDaemon(true)
  }

  def start() = thread.start()

  def stop() = thread.interrupt()
}
