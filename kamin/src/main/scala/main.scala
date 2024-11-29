package kamin

import basic.BasicEvaluator
import org.jline.reader.{LineReader, LineReaderBuilder}
import org.jline.terminal.TerminalBuilder

import scala.util.control.Breaks.{break, breakable}

private def isBalanced(input: String): Boolean =
  var stack = List[Char]()

  breakable {
    for (char <- input)
      char match
        case '(' => stack = char :: stack // Push opening parenthesis to stack
        case ')' if stack.isEmpty || stack.head != '(' => break() // Unmatched closing parenthesis
        case ')' => stack = stack.tail // Pop the matching opening parenthesis
        case _ => // Ignore other characters
  }
  stack.isEmpty // If stack is empty, all parentheses were balanced

implicit class StringExtensions(val s: String) extends AnyVal:
  def removeComment(): String = s.takeWhile(_ != ';')

@main
def main(): Unit =
  val terminal = TerminalBuilder.terminal()
  val lineReader = LineReaderBuilder.builder().terminal(terminal).build()
  var evaluator: Evaluator = BasicEvaluator()

  RegistriesSetup.initialize()

  def error(e: String): Unit =
    println("Error: " + e)

  var continue = true
  while continue do
    var input = lineReader.readLine("->").removeComment()

    input match
      case "exit" => continue = false
      case "basic" => evaluator = BasicEvaluator()
      case _ =>
        while !isBalanced(input) do
          input = input + " " + lineReader.readLine(">").removeComment()

        println(evaluator.evaluate(input))

