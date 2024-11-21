import scala.collection.mutable

sealed trait Environment:
  def get(key: String): Option[Value]
  def set(key: String, value: Value): Unit
  def openScope(keys: Seq[String]): Unit
  def closeScope(): Unit

class GlobalAndLocalScopeEnvironment extends Environment:
  private val globalScope = mutable.HashMap[String, Option[Value]]()
  private val localScopes = mutable.Stack[mutable.Map[String, Option[Value]]]()

  override def openScope(keys: Seq[String]): Unit =
    val scope: mutable.Map[String, Option[Value]] = mutable.Map.from(keys.map(_ -> None))
    localScopes.push(scope)

  override def closeScope(): Unit =
    if localScopes.nonEmpty then localScopes.pop()
    else throw new IllegalStateException("Cannot close the global scope")

  override def get(key: String): Option[Value] =
    localScopes.collectFirst { case scope if scope.contains(key) => scope(key) }
      .flatten
      .orElse(globalScope.get(key).flatten)

  override def set(key: String, value: Value): Unit =
    localScopes.headOption match
      case Some(scope) => 
        if scope.contains(key) then scope.put(key, Some(value)) else globalScope.put(key, Some(value))
      case _ => globalScope.put(key, Some(value))
