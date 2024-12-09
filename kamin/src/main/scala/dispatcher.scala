package kamin

import kamin.Value

import scala.reflect.TypeTest

trait Dispatcher[Op[_ <: Value, _ <: Value]]:
  def dispatch(v1: Value, v2: Value): Option[Op[Value, Value]]
  def orElse(other: Dispatcher[Op]): Dispatcher[Op] =
    (v1: Value, v2: Value) =>
      this.dispatch(v1, v2).orElse(other.dispatch(v1, v2))

object Dispatcher:
  def create[T1 <: Value, T2 <: Value, Op[_ <: Value, _ <: Value]](using
                                                                   tt1: TypeTest[Value, T1],
                                                                   tt2: TypeTest[Value, T2],
                                                                   operation: Op[T1, T2]
                                                                  ): Dispatcher[Op] =
    new Dispatcher[Op]:
      def dispatch(v1: Value, v2: Value): Option[Op[Value, Value]] =
        for
          t1 <- tt1.unapply(v1)
          t2 <- tt2.unapply(v2)
        yield operation.asInstanceOf[Op[Value, Value]]

