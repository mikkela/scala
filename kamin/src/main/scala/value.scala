package kamin

import scala.util.{Failure, Success, Try}

trait Value:
  def isTrue: Boolean

case class IntegerValue(value: Int) extends Value:
  override def isTrue: Boolean = value != 0

  override def toString: String = value.toString

object IntegerValue:
  val intTrue = 1
  val intFalse = 0
  val True: IntegerValue = IntegerValue(intTrue)
  val False: IntegerValue = IntegerValue(intFalse)

trait IntegerValueReader extends Reader:
  override def read(input: String): Either[String, Value] =
    Try(input.toInt) match {
      case Success(number) => Right(IntegerValue(number))
      case Failure(_) => super.read(input)
    }
