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
  given parserContext: BasicLanguageFamilyParserContext = BasicParserContext
  given environment: Environment = GlobalAndLocalScopeEnvironment()
  given functionDefinitionTable: FunctionDefinitionTable = FunctionDefinitionTable()

  def error(e: String): Unit =
    println("Error: " + e)

  def registerFunction(f: FunctionDefinitionNode): Unit =
    functionDefinitionTable.register(f)
    println(f.function)

  def evaluateExpression(e: ExpressionNode): Unit =
    e.evaluate match
      case Left(e) => error(e)
      case Right(value) => println(value)
  var continue = true
  while continue do
    var input = lineReader.readLine("->").removeComment()

    if (input == "exit") continue = false
    else
      while !isBalanced(input) do
        input = input + " " + lineReader.readLine(">").removeComment()

      BasicParser.parse(PeekingIterator[Token](BasicLexer.tokens(input))) match
        case Left(e) => error(e)
        case Right(f: FunctionDefinitionNode) =>
          registerFunction(f)
        case Right(e: ExpressionNode) =>
          evaluateExpression(e)

