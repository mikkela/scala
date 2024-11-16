import org.jline.reader.{LineReader, LineReaderBuilder}
import org.jline.terminal.TerminalBuilder

import scala.util.control.Breaks.{break, breakable}

def isBalanced(input: String): Boolean =
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
@main
def main(): Unit = {
  val terminal = TerminalBuilder.terminal()
  val lineReader = LineReaderBuilder.builder().terminal(terminal).build()

  given parserContext: BasicLanguageFamilyParserContext = BasicParserContext
  var continue = true
  while continue do
    var input = lineReader.readLine("->").removeComment()

    if (input == "exit") continue = false
    else
      while !isBalanced(input) do
        input = input + " " + lineReader.readLine(">").removeComment()

      val ast = BasicParser.parse(PeekingIterator[Token](BasicLexer.tokens(input)))
      println(ast)
}

