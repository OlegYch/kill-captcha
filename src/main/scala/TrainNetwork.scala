import collection.GenSeq
import java.io.File
import java.lang.String
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import org.encog.ml.data.basic.BasicMLDataSet
import org.encog.ml.data.MLDataSet
import org.encog.ml.train.MLTrain
import org.encog.neural.networks.BasicNetwork
import org.encog.neural.networks.layers.BasicLayer
import org.encog.neural.networks.training.propagation.resilient.ResilientPropagation
import org.encog.neural.networks.training.propagation.TrainingContinuation
import org.encog.util.obj.SerializeObject

class TrainNetwork(val network: BasicNetwork,
                   var defTrainingContinuation: Option[TrainingContinuation] = None) {
  def this(dim: Int = NetworkConstants.inputDimensions, outDim: Int = NetworkConstants.outputDimensions) {
    this (new BasicNetwork() {
      addLayer(new BasicLayer(dim))
      addLayer(new BasicLayer(dim * 2))
      addLayer(new BasicLayer(outDim))
      getStructure.finalizeStructure()
      reset()
    })
  }

  lazy val inputDimensions = network.getLayerNeuronCount(0)
  lazy val outputDimensions = network.getLayerNeuronCount(network.getLayerCount - 1)

  import SplitImage.withSafeFiles

  def set: Seq[(File, String)] = new File(NetworkConstants.dataDir).listFilesSafe
    .filter(_.getName.length == 1).flatMap {
    f => f.listFilesSafe.map(image => (image, f.getName))
  }

  def readImageInput(image: BufferedImage): Array[Double] = {
    image.getRGB(0, 0, image.getWidth, image.getHeight, newInputArray, 0, image.getWidth).map(_.toDouble.abs)
  }

  def input(set: GenSeq[(File, String)]): GenSeq[Array[Double]] = for {
    sample <- set
  } yield {
    val image = ImageIO.read(sample._1)
    readImageInput(image)
  }

  def sampleToData(sample: String): Array[Double] = {
    val answer = sample.toInt
    val output = newResultArray
    output(answer) = 1.0
    output
  }

  def ideal(set: GenSeq[(File, String)]): GenSeq[Array[Double]] = for {
    sample <- set
  } yield {
    sampleToData(sample._2)
  }

  def newInputArray: Array[Int] = Array.ofDim[Int](inputDimensions)

  def newResultArray = Array.ofDim[Double](outputDimensions)

  def compute(input: Array[Double]): (Double, Int) = {
    val result = newResultArray
    network.compute(input, result)
//    println("computed = " + result.mkString(" "))
    result.zipWithIndex.maxBy(_._1)
  }

  def trainNetwork(set: GenSeq[(File, String)],
                   trainingContinuation: Option[TrainingContinuation] = defTrainingContinuation) = {
    if (set.size > 0) {
      val trainingSet = set.par
      val samples: MLDataSet = new BasicMLDataSet(input(trainingSet).toArray, ideal(trainingSet).toArray)
      val train: MLTrain = new ResilientPropagation(network, samples)
      trainingContinuation.map(train.resume(_))
      train.iteration()
      defTrainingContinuation = Some(train.pause())
      train.getError
    } else {
      1.0
    }
  }

  case class TestResults(testResults: GenSeq[Boolean]) {
    val successRate = testResults.filter(_ == true).size.toDouble / testResults.size * 100
  }

  def testNetwork(testSet: GenSeq[(File, String)]): TestResults = {
    TestResults(input(testSet).zip(ideal(testSet)).map {
      case (input, ideal) =>
//        println("ideal = " + ideal.mkString(" "))
        val selected = compute(input)
        ideal(selected._2) == 1.0
    })
  }


  def contents: TrainNetwork.Contents = {
    (network, defTrainingContinuation)
  }

  def save = {
    org.encog.util.obj.SerializeObject.save(NetworkConstants.networkFile, contents)
    this
  }
}

object TestNetwork extends App {
  type Contents = (BasicNetwork, Option[TrainingContinuation])

  val tn = TrainNetwork.load

  import tn._

  val results = for (char <- '0' to '9') yield new {
    val results = testNetwork(set
      .filter {case (f, answer) => answer == char.toString}
    )
    val input = char
  }
  results.map(t => println("success rate for " + t.input + " =  " + t.results.successRate))
}

object TrainNetwork extends App {
  type Contents = (BasicNetwork, Option[TrainingContinuation])

  new TrainNetwork() {
    val (trainingSet, testSet) = {
      val inputSet = set
      val size = inputSet.size
      //      val size = 100
      util.Random.shuffle(inputSet).take(size).splitAt(size * 9 / 10)
    }
    println("training size " + trainingSet.size)
    println("test size " + testSet.size)
    var iteration = 0
    while (true) {
      println("Iteration = " + iteration)
      iteration += 1
      println("train error = " + trainNetwork(trainingSet))
      save
      println("success rate = " + testNetwork(testSet).successRate)
    }
    sys.exit()
  }

  def load = {
    val contents = SerializeObject.load(NetworkConstants.networkFile).asInstanceOf[Contents]
    new TrainNetwork(contents._1, contents._2)
  }
}

object NetworkConstants {
  val dataDir = "data/"
  val networkFile = new File("network")
  val imageDimensions = 10
  val inputDimensions = imageDimensions * imageDimensions
  val outputDimensions = 10
}
