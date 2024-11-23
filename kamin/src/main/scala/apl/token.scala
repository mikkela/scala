package kamin.apl

import kamin.Token

object MaxToken extends Token:
  override def literal: String = "max"
object AndToken extends Token:
  override def literal: String = "and"
object OrToken extends Token:
  override def literal: String = "or"
object PlusSlashToken extends Token:
  override def literal: String = "+/"
object MinusSlashToken extends Token:
  override def literal: String = "-/"
object AsteriskSlashToken extends Token:
  override def literal: String = "*/"
object SlashSlashToken extends Token:
  override def literal: String = "//"
object MaxSlashToken extends Token:
  override def literal: String = "max/"
object OrSlashToken extends Token:
  override def literal: String = "or/"
object AndSlashToken extends Token:
  override def literal: String = "and/"
object CompressToken extends Token:
  override def literal: String = "compress"
object ShapeToken extends Token:
  override def literal: String = "shape"
object RavelToken extends Token:
  override def literal: String = "ravel"
object RestructToken extends Token:
  override def literal: String = "restruct"
object CatToken extends Token:
  override def literal: String = "cat"
object IndxToken extends Token:
  override def literal: String = "indx"
object TransToken extends Token:
  override def literal: String = "trans"
object SquareBracketsToken extends Token:
  override def literal: String = "[]"