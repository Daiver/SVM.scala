
//import scala.io._
import java.io._
import breeze.linalg._

//TODO: Should use scala.io 
package object InputData {
  def readMNISTImages(fname: String): DenseMatrix[Double] = {
    //assert ( false ) //Not implemented yet
    var in = new DataInputStream(new BufferedInputStream(new FileInputStream(fname)))
    //var in = Source.fromFile(fname)
    val magic = in.readInt
    if(magic != 2051){
      println(s"Magic not equal to 2051! $magic")
      System.exit(1)
    }
    val nImages = in.readInt
    val nRows   = in.readInt
    val nCols   = in.readInt
    //println(s"Num of images $countOfImages rows $countOfRows cols $countOfCols")

    DenseMatrix.tabulate[Double](nCols * nRows, nImages) {
      case(i, j) => in.readUnsignedByte.toDouble
    } t
  }

  def readMNISTLabels(fname: String): DenseVector[Int] = {
    var in = new DataInputStream(new BufferedInputStream(new FileInputStream(fname)))
    val magic = in.readInt
    if(magic != 2049){
      println(s"Magic not equal to 2049! $magic")
      System.exit(1)
    }
    val nLabels = in.readInt
    DenseVector.tabulate(nLabels){i => in.readUnsignedByte()}
  }

}
