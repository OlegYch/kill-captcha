import collection.GenSeq
import java.io.File
import java.lang.String
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import org.encog.ml.data.basic.BasicMLDataSet
import org.encog.ml.train.MLTrain
import org.encog.neural.networks.BasicNetwork
import org.encog.neural.networks.layers.BasicLayer
import org.encog.neural.networks.training.propagation.resilient.ResilientPropagation

class TrainNetwork extends NetworkConstants {
  val network: BasicNetwork = new BasicNetwork()
  network.addLayer(new BasicLayer(inputDimensions))
  network.addLayer(new BasicLayer(inputDimensions / 3))
  network.addLayer(new BasicLayer(outputDimensions))
  network.getStructure.finalizeStructure()
  network.reset()

  import SplitImage.withSafeFiles

  def set: GenSeq[(File, String)] = util.Random.shuffle(new File(dataDir).listFilesSafe
    .filter(_.getName.length == 1).flatMap {
    f => f.listFilesSafe.map(image => (image, f.getName)).take(1350)
  }).par

  def readImageInput(image: BufferedImage): Array[Double] = {
    image.getRGB(0, 0, image.getWidth, image.getHeight, newInputArray, 0, image.getWidth).map(_.toDouble.abs)
  }

  def input(set: GenSeq[(File, String)]): GenSeq[Array[Double]] = for {
    sample <- set
  } yield {
    val image = ImageIO.read(sample._1)
    readImageInput(image)
  }

  def ideal(set: GenSeq[(File, String)]): GenSeq[Array[Double]] = for {
    sample <- set
  } yield {
    val answer = sample._2.toInt
    val output = newResultArray
    output(answer) = 1.0
    output
  }

  def newInputArray: Array[Int] = Array.ofDim[Int](inputDimensions)

  def newResultArray = Array.ofDim[Double](outputDimensions)

  def compute(input: Array[Double], network: BasicNetwork = network): (Double, Int) = {
    val result = newResultArray
    network.compute(input, result)
    println("computed = " + result.mkString(" "))
    result.zipWithIndex.maxBy(_._1)
  }

  def trainNetwork(trainingSet: GenSeq[(File, String)]) {
    val train: MLTrain = new ResilientPropagation(network,
      new BasicMLDataSet(input(trainingSet).toArray, ideal(trainingSet).toArray))
    for (i <- 1 to 100) {
      train.iteration()
      println(i + " " + train.getError)
    }
  }

  def testNetwork(testSet: GenSeq[(File, String)]): GenSeq[Boolean] = {
    input(testSet).zip(ideal(testSet)).map {
      case (input, ideal) =>
        println("ideal = " + ideal.mkString(" "))
        val selected = compute(input)
        ideal(selected._2) == 1.0
    }
  }
}

object TrainNetwork extends TrainNetwork with App {
  val (trainingSet, testSet) = set.splitAt(set.size * 9 / 10)
  trainNetwork(trainingSet)
  val testResults = testNetwork(testSet)
  println("success rate = " + testResults.filter(_ == true).size.toDouble / testResults.size * 100)
  //  SerializeObject.save(KillCaptcha.networkFile, network)
  sys.exit()
}

trait NetworkConstants {
  val dataDir = "target/data/"
  val imageDimensions = 15
  val inputDimensions = imageDimensions * imageDimensions
  val outputDimensions = 10
}
