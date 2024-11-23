package kamin

import scala.collection.mutable

class FunctionDefinitionTable:
  private val table = mutable.HashMap[String, FunctionDefinitionNode]()

  def register(functionDefinition: FunctionDefinitionNode): Unit =
    table.put(functionDefinition.function, functionDefinition)

  def lookupFunctionDefinition(name: String): Option[FunctionDefinitionNode] =
    table.get(name)

