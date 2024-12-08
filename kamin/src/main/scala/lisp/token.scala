package kamin.lisp

import kamin.Token

object ConsToken extends Token:
  override def literal: String = "cons"

object CarToken extends Token:
  override def literal: String = "car"

object CdrToken extends Token:
  override def literal: String = "cdr"

object NumberTestToken extends Token:
  override def literal: String = "number?"

object SymbolTestToken extends Token:
  override def literal: String = "symbol?"

object ListTestToken extends Token:
  override def literal: String = "list?"

object NullTestToken extends Token:
  override def literal: String = "null?"
