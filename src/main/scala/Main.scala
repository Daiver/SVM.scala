
import scala.util.Random
import breeze.linalg._
import breeze.stats.distributions._
import ml._
import optimization._
import utils._
import ml.OneVsAllTrain._

import InputData._

import breeze.plot._;

object App {

  def main(args: Array[String]) {
    println("Start!")

    val mnistFolder = "/home/daiver/coding/data/mnist/"
    val trainImages = readMNISTImages (mnistFolder + "train-images-idx3-ubyte")
    val trainLabels = readMNISTLabels (mnistFolder + "train-labels-idx1-ubyte")
    val testImages  = readMNISTImages (mnistFolder + "t10k-images-idx3-ubyte")
    val testLabels  = readMNISTLabels (mnistFolder + "t10k-labels-idx1-ubyte")

    val clf = trainOneVsAllClassifier(trainImages, trainLabels, 10)
    val errTrain = clf.evaluate(trainImages, trainLabels)
    val errTest  = clf.evaluate(testImages, testLabels)
    println("train error " + (100.0 * errTrain.toDouble / trainLabels.length.toDouble) + "%")
    println("test error  " + (100.0 * errTest.toDouble / testLabels.length.toDouble) + "%")
    println("Finish!")
  }

}

