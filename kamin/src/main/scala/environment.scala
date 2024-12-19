package kamin

import scala.collection.mutable

sealed trait Environment:
  def get(key: String): Option[Value]
  def set(key: String, value: Value): Unit

object GlobalEnvironment extends Environment:
  private val scope = mutable.HashMap[String, Option[Value]]()

  override def get(key: String): Option[Value] =
    scope.get(key).flatten

  override def set(key: String, value: Value): Unit =
    scope.put(key, Some(value))

class EnvironmentFrame(private val previousEnvironment: Environment, keys: Seq[String]) extends Environment:
  private val scope: mutable.Map[String, Option[Value]] = mutable.Map.from(keys.map(_ -> None))
  
  override def get(key: String): Option[Value] =
    if scope.contains(key) then scope(key) else previousEnvironment.get(key)

  override def set(key: String, value: Value): Unit =
    if scope.contains(key) then scope.put(key, Some(value)) else previousEnvironment.set(key, value)
