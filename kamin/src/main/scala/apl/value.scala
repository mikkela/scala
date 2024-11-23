package kamin.apl

import kamin.{Reader, Value}

import scala.annotation.targetName
import scala.util.{Failure, Success, Try}

case class MatrixValue(value: Vector[Vector[Int]]) extends Value:
  require(
    value.nonEmpty && value.forall(_.length == value.head.length),
    "All rows in the matrix must have the same number of columns and be non-empty."
  )
  override def isTrue: Boolean = !value.forall(_.forall(_ == 0))
  override def toString: String = value.map(_.mkString(" ")).mkString("\n")
object MatrixValue:
  def fromVector(vector: Vector[Int]): MatrixValue = MatrixValue(Vector(vector))

trait MatrixValueReader extends Reader:
  override def read(input: String): Either[String, Value] =
    if input.startsWith("'") && input.drop(1).trim.startsWith("(") && input.endsWith(")") then
      val content = input.drop(1).trim.drop(1).dropRight(1) // Remove `'` and parentheses
      Try(content.split("\\s+").map(_.toInt).toVector) match
        case Success(value) => Right(MatrixValue.fromVector(value))
        case Failure(_) => super.read(input)
    else
      super.read(input)
