package kamin.apl

import kamin.{Value, IntegerValue}
object ValueExtensions {
  extension (value: Value)
    def shape: Either[String, VectorValue] =
      value match
        case IntegerValue(i) => Right(VectorValue.emptyVector)
        case VectorValue(v) => Right(VectorValue.createVector(Seq(v.length)))
        case MatrixValue(m) => Right(VectorValue.createVector(Seq(m.length, m.head.length)))
        case v => Left(s"Unable to determine shape of an unknown type: ${v.getClass.getName}")
}
