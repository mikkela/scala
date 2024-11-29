/*package kamin

import scala.reflect.TypeTest

// Dispatcher for dynamic operation resolution
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
    (v1: Value, v2: Value) =>
      for
        t1 <- tt1.unapply(v1)
        t2 <- tt2.unapply(v2)
      yield operation.asInstanceOf[Op[Value, Value]]

// Configurable and clearable DispatcherRepository
class DispatcherRepository[Op[_ <: Value, _ <: Value]](initialDispatchers: List[Dispatcher[Op]] = List.empty):
  private var dispatchers: List[Dispatcher[Op]] = initialDispatchers

  /** Register a new dispatcher */
  def register[T1 <: Value, T2 <: Value](using
                                         tt1: TypeTest[Value, T1],
                                         tt2: TypeTest[Value, T2],
                                         operation: Op[T1, T2]
                                        ): Unit =
    val dispatcher = Dispatcher.create[T1, T2, Op]
    dispatchers = dispatcher :: dispatchers

  /** Add an external dispatcher */
  def addDispatcher(dispatcher: Dispatcher[Op]): Unit =
    dispatchers = dispatcher :: dispatchers

  /** Dispatch a pair of values using the registered dispatchers */
  def dispatch(v1: Value, v2: Value): Option[Op[Value, Value]] =
    dispatchers.iterator.map(_.dispatch(v1, v2)).collectFirst { case Some(op) => op }

  /** Clear all registered dispatchers */
  def clear(): Unit =
    dispatchers = List.empty

object EvaluatorRegistry:
  private val evaluators: scala.collection.mutable.Map[Class[_], ExpressionEvaluator[_]] = scala.collection.mutable.Map()

  /** Register an evaluator */
  def register[T <: ExpressionNode](cls: Class[T], evaluator: ExpressionEvaluator[T]): Unit =
    evaluators(cls) = evaluator

  /** Get an evaluator by class */
  def getEvaluator[T <: ExpressionNode](cls: Class[T]): Option[ExpressionEvaluator[T]] =
    evaluators.get(cls).asInstanceOf[Option[ExpressionEvaluator[T]]]

  /** Clear all registered evaluators */
  def clear(): Unit =
    evaluators.clear()


*/
