
//import scala.io._
import java.io._
import breeze.linalg._
import breeze.math._
import scala.reflect.ClassTag
import utils._

//TODO: Should use scala.io 
package object InputData {

  trait Caster[@specialized(Double) T]{
    def cast(v: Int): T
  }

  class DoubleCaster extends Caster[Double]{
    def cast(v: Int): Double = v.toDouble
  }
  
  object Casters {
    implicit val doubleCaster = new DoubleCaster
  }

  def cast[@specialized(Double) T: Caster](v: Int) = implicitly[Caster[T]].cast(v)

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

/*  def readMNISTLabels[T: ClassTag](fname: String): DenseVector[T] = {*/
    //var in = new DataInputStream(new BufferedInputStream(new FileInputStream(fname)))
    //val magic = in.readInt
    //if(magic != 2049){
      //println(s"Magic not equal to 2049! $magic")
      //System.exit(1)
    //}
    //val nLabels = in.readInt
    //DenseVector.tabulate[T](nLabels){i => convertInt[T](in.readUnsignedByte)}
  //}

}
