
import breeze.linalg._
import scala.reflect.runtime.universe._

package object utils {

  def subsetByLabel(data: DenseMatrix[Double], labels: DenseVector[Int], label: Int): DenseMatrix[Double] = {
    val nRows = data.rows
    val nCols = data.cols
    assert(nRows == labels.length)
    val indices   = (0 until nRows) filter {x => labels(x) == label}
    val nIndices  = indices.length
    var resData   = DenseMatrix.zeros[Double](nIndices, nCols)
    for(i <- 0 until nIndices) {
      val index = indices(i)
      resData(i, ::) := data(index, ::)
    }
    resData
  }

  def time[R](block: => R): R = {  
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      println("Elapsed time: " + (t1 - t0) * 1e-9 + "s")
      result
  }

  //trait FromIntConverter[T]{
	//def apply(int: Int): T
  //}

  //def convertInt[T](int: Int)(implicit converter: FromIntConverter[T]): T = converter(int)

  //implicit val IntToIntConverter = new FromIntConverter[Int]{
	//def apply(int: Int) = int
  //}

  //implicit val IntToFloatConverter = new FromIntConverter[Float]{
	//def apply(int: Int) = int.toFloat
  //}

  //implicit val IntToDoubleConverter = new FromIntConverter[Double]{
	//def apply(int: Int) = int.toDouble
  //}

}
