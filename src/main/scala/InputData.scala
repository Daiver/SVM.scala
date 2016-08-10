
//import scala.io._
import java.io._
import breeze.linalg._
import breeze.math._
import scala.reflect.ClassTag

//TODO: Should use scala.io 
package object InputData {

  def readMNISTImages(fname: String): DenseMatrix[Double] = {
    var in = new DataInputStream(new BufferedInputStream(new FileInputStream(fname)))
    val magic = in.readInt
    if(magic != 2051){
      println(s"Magic not equal to 2051! $magic")
      System.exit(1)
    }
    val nImages = in.readInt
    val nRows   = in.readInt
    val nCols   = in.readInt

    DenseMatrix.tabulate(nCols * nRows, nImages) {(i, j) => in.readUnsignedByte.toDouble} t
  }

  def readMNISTLabels(fname: String): DenseVector[Int] = {
    var in = new DataInputStream(new BufferedInputStream(new FileInputStream(fname)))
    val magic = in.readInt
    if(magic != 2049){
      println(s"Magic not equal to 2049! $magic")
      System.exit(1)
    }
    val nLabels = in.readInt
    DenseVector.tabulate(nLabels){i => in.readUnsignedByte}
  }

/*  def readMNISTImages[T: ClassTag:Semiring](fname: String): DenseMatrix[T] = {*/
    //var in = new DataInputStream(new BufferedInputStream(new FileInputStream(fname)))
    //val magic = in.readInt
    //if(magic != 2051){
      //println(s"Magic not equal to 2051! $magic")
      //System.exit(1)
    //}
    //val nImages = in.readInt
    //val nRows   = in.readInt
    //val nCols   = in.readInt

    //DenseMatrix.tabulate(nCols * nRows, nImages) {(i, j) => in.readUnsignedByte.asInstanceOf[T]} t
  //}

  //def readMNISTLabels[T: ClassTag](fname: String): DenseVector[T] = {
    //var in = new DataInputStream(new BufferedInputStream(new FileInputStream(fname)))
    //val magic = in.readInt
    //if(magic != 2049){
      //println(s"Magic not equal to 2049! $magic")
      //System.exit(1)
    //}
    //val nLabels = in.readInt
    //DenseVector.tabulate(nLabels){i => in.readUnsignedByte}
  //}

}
