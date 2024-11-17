
trait Token:
  def literal: String

case class NameToken(literal: String) extends Token
case class IntegerValueToken(literal: String) extends Token
case class IllegalToken(literal: String) extends Token

object LeftParenthesisToken extends Token:
  override def literal: String = "("
object RightParenthesisToken extends Token:
  override def literal: String = ")"
object EqualToken extends Token:
  override def literal: String = "="
object LessThanToken extends Token:
  override def literal: String = "<"
object GreaterThanToken extends Token:
  override def literal: String = ">"
object PlusToken extends Token:
  override def literal: String = "+"
object MinusToken extends Token:
  override def literal: String = "-"
object AsteriskToken extends Token:
  override def literal: String = "*"
object SlashToken extends Token:
  override def literal: String = "/"
object DefineToken extends Token:
  override def literal: String = "define"
object PrintToken extends Token:
  override def literal: String = "print"
object ReadToken extends Token:
  override def literal: String = "read"
object IfToken extends Token:
  override def literal: String = "if"
object WhileToken extends Token:
  override def literal: String = "while"
object SetToken extends Token:
  override def literal: String = "set"
object BeginToken extends Token:
  override def literal: String = "begin"

