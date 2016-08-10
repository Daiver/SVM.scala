
import ml.SVM
import breeze.linalg._
import utils._
import optimization.SGD

//Whole package should be rewrited!
package ml {
  //Should be rewrited!
  case class PairwiseClassifier(ind1: Int, ind2: Int, svm: SVM){
	def predict(sample: DenseVector[Double]): Int = {
	  val svmResp = svm.predict(sample)
	  if(svmResp == -1)
		ind1
	  else
		ind2
	}
  }

  case class OneVsAllClassifier(classifiers: List[PairwiseClassifier], nClasses: Int) {

	def predictTable(sample: DenseVector[Double]) = {
	  var table = DenseVector.zeros[Int](nClasses)
	  for(clf <- classifiers){
		val res = clf.predict(sample)
		if(res != -1)
		  table(res) += 1
	  }
	  table
	}

	def predict(sample: DenseVector[Double]): Int = predictTable(sample).argmax
	

	def predict(samples: DenseMatrix[Double]): DenseVector[Int] = {
	  var res = DenseVector.zeros[Int](samples.rows)
	  //Rewrite it
	  for(i <- 0 until samples.rows){
		res(i) = predict(samples(i, ::).t)
	  }
	  res
	}

	def evaluate(samples: DenseMatrix[Double], labels: DenseVector[Int]): Int = 
	  (predict(samples) :== labels) map {x => if(x) 0 else 1} sum
  }

  package object OneVsAllTrain {

	def trainPairwiseClassifier(data: DenseMatrix[Double], labels: DenseVector[Int], ind1: Int, ind2: Int) = {
	  val wInit = DenseVector.ones[Double](data.cols + 1) :/ data.cols.toDouble

	  val data1   = subsetByLabel(data, labels, ind1)
	  val data2   = subsetByLabel(data, labels, ind2)
	  val labels1 = DenseVector.ones[Int](data1.rows) * -1
	  val labels2 = DenseVector.ones[Int](data2.rows) 

	  val dataTrain   = DenseMatrix.vertcat(data1, data2)
	  val labelsTrain = DenseVector.vertcat(labels1, labels2)

	  val batchSize = 500
	  val momentCoeff = 2.5
	  val learningRate = 0.0001
	  val nIters = 10
	  val lambda = 0.003
	  val weights = time { SGD(
		  svmError, svmGradient(lambda, _, _, _), 
		  dataTrain, labelsTrain, wInit, 
		  nIters, batchSize, learningRate, momentCoeff) }
	  val svm = SVM(weights(0 to weights.length - 2), weights(weights.length - 1))
	  PairwiseClassifier(ind1, ind2, svm)
	}

	//More classifiers for the Classifier God!
	def trainOneVsAllClassifier(data: DenseMatrix[Double], labels: DenseVector[Int], nClasses: Int) = {
	  var classifiers: List[PairwiseClassifier] = List()
	  for(ind1 <- 0 until nClasses){
		for(ind2 <- ind1 until nClasses){
		  if(ind1 != ind2){
			println("train " + ind1 + "-" + ind2)
			classifiers = classifiers :+ trainPairwiseClassifier(data, labels, ind1, ind2)
			println(classifiers.length)
		  }
		}
	  }
	  OneVsAllClassifier(classifiers, nClasses)
	}

  }

}
