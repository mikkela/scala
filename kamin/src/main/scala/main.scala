import org.jline.reader.{LineReader, LineReaderBuilder}
import org.jline.terminal.TerminalBuilder

def isBalanced(input: String): Boolean =
  var stack = List[Char]()

  for (char <- input)
    char match
      case '(' => stack = char :: stack // Push opening parenthesis to stack
      case ')' =>
        if (stack.isEmpty || stack.head != '(') return false // Unmatched closing parenthesis
        stack = stack.tail // Pop the matching opening parenthesis
      case _ => // Ignore other characters

  stack.isEmpty // If stack is empty, all parentheses were balanced
@main
def main(): Unit = {
  val terminal = TerminalBuilder.terminal()
  val lineReader = LineReaderBuilder.builder().terminal(terminal).build()

  var continue = true
  while continue do
    var input = lineReader.readLine("->")

    if (input == "exit") continue = false
    else
      while !isBalanced(input) do
        input = input + lineReader.readLine(">")

      println("Entered: " + input)
      BasicLexer.tokens(input).foreach(println)
}

