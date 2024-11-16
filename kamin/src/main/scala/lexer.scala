class Lexer(separators: => Seq[Token], keywords: => Seq[Token]):
  def tokens(input: String):Iterator[Token] =
    var position = 0

    def isInteger(s: String): Boolean = s.head == '-' && s.tail.forall(_.isDigit) || s.forall(_.isDigit)

    def isName(s: String): Boolean = s.forall(c => c.isLetter || !isSeparator(c))

    def currentChar: Char =
      if position < input.length then input(position) else '\u0000'

    def nextChar: Char =
      if position + 1 < input.length then input(position + 1) else '\u0000'

    def isSeparator(c: Char): Boolean =
      c == '\u0000' || c.isWhitespace || separators.exists(_.literal == c.toString)

    def advance(): Unit = position += 1

    def isEndOfLine: Boolean =
      position >= input.length

    def skipWhitespaces(): Unit =
      while currentChar.isWhitespace do advance()

    def toToken(text: String): Token =
      text match
        case _ if separators.exists(_.literal == text) => separators.find(_.literal == text).get
        case _ if keywords.exists(_.literal == text) => keywords.find(_.literal == text).get
        case _ if isInteger(text) => IntegerValueToken(text)
        case _ if isName(text) => NameToken(text)
        case _ => IllegalToken(text)

    def lexToken(): Token =
      val start = position
      while !isSeparator(currentChar) do advance()
      val text = input.substring(start, position)
      toToken(text)

    new Iterator[Token]:
      override def hasNext: Boolean =
        skipWhitespaces()
        !isEndOfLine

      override def next(): Token =
        skipWhitespaces()

        currentChar match
          case c if separators.exists(_.literal == c.toString) =>
            advance()
            toToken(c.toString)
          case _ => lexToken()
