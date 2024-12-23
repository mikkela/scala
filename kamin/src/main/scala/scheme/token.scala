package kamin.scheme

import kamin.Token

object LambdaToken extends Token:
  override def literal: String = "lambda"
  
object PrimopTestToken extends Token:
  override def literal: String = "primop?"

object ClosureTestToken extends Token:
  override def literal: String = "closure?"