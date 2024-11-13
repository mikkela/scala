object BasicLexer extends Lexer(
  Seq(LeftParenthesisToken, RightParenthesisToken),
  Seq(EqualToken, LessThanToken, GreaterThanToken, PlusToken, MinusToken, AsteriskToken, SlashToken, PrintToken,
    DefineToken, IfToken, WhileToken, SetToken, BeginToken)
)
