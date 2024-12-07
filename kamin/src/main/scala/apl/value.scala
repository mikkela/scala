package kamin.apl

import kamin.{IntegerValue, Reader, Value}
import scala.util.{Failure, Success, Try}

case class VectorValue private(value: Vector[Int]) extends Value:
  override def isTrue: Boolean = this.value.exists(_ != 0)
  override def toString: String = value.mkString(" ") + "\n"

object VectorValue:
  def createVector(values: Seq[Int]): VectorValue =
    if (values.isEmpty)
      emptyVector
    else
      VectorValue(values.toVector)

  def emptyVector: VectorValue = VectorValue(Vector.empty)

case class MatrixValue private(value: Vector[Vector[Int]]) extends Value:
  override def isTrue: Boolean = value.exists(_.exists(_ != 0))
  override def toString: String = value.map(_.mkString(" ")).mkString("\n")

object MatrixValue:
  def createMatrix(values: Seq[Seq[Int]]): MatrixValue =
    if (values.isEmpty || values.head.isEmpty) then
      emptyMatrix
    else
      MatrixValue(values.map(_.toVector).toVector)

  def emptyMatrix: MatrixValue = MatrixValue(Vector.empty)

trait VectorValueReader extends Reader:
  override def read(input: String): Either[String, Value] =
    if input.startsWith("'") && input.drop(1).trim.startsWith("(") && input.endsWith(")") then
      val content = input.drop(1).trim.drop(1).dropRight(1) // Remove `'` and parentheses
      Try(content.split("\\s+").map(_.toInt).toVector) match
        case Success(value) => Right(VectorValue.createVector(value))
        case Failure(_) => super.read(input)
    else
      super.read(input)
