package record_calculus.parsers

import scala.util.parsing.combinator._

import record_calculus._

trait LambdaCalculusParsers extends JavaTokenParsers {
  type CalculusType <: LambdaCalculus

  val calculus: CalculusType
  import calculus._

  def variable: Parser[Var] =
    ident ^^ Var

  def lambda: Parser[Lambda] =
    ("\\" ~>! variable ~! ("." ~>! term)) ^^ { case v ~ b => Lambda(v, b) }

  def singleTerm: Parser[Term] =
    lambda | variable | "(" ~> term <~ ")"

  def safeWithoutParens: Parser[Term] = variable

  def term: Parser[Term] =
    rep1(singleTerm) ^^ { case terms => terms.reduceLeft(App) }
}

trait RecordParsers extends LambdaCalculusParsers {
  self: LambdaCalculusParsers =>

  type CalculusType <: LambdaCalculus with Records
  import calculus._

  def member: Parser[(String, Term)] =
    ident ~! ("=" ~>! term) ^^ { case l ~ m => (l, m) }

  def record: Parser[Record] =
    "{" ~>! repsep(member, ",") <~ "}" ^^ (labels => Record(Map() ++ labels))

  override def safeWithoutParens: Parser[Term] =
    record | super.safeWithoutParens

  def get: Parser[Get] =
    ("(" ~> term <~ ")" | safeWithoutParens) ~ ("." ~>! ident) ^^ { case t ~ l => Get(t, l) }

  override def singleTerm: Parser[Term] =
    get | record | super.singleTerm
}

trait MergeParsers extends RecordParsers {
  self: LambdaCalculusParsers with RecordParsers =>

  type CalculusType <: LambdaCalculus with Records with Merge
  import calculus._

  def merge: Parser[Merge] =
    ("(" ~> term <~ ")" | safeWithoutParens) ~ ("<+>" ~>! record) ^^ { case m ~ r => Merge(m, r) }

  override def singleTerm: Parser[Term] = merge | super.singleTerm
}

trait PrimitiveArithmeticParsers extends LambdaCalculusParsers {
  self: LambdaCalculusParsers =>

  type CalculusType <: LambdaCalculus with PrimitiveArithmetic
  import calculus._

  def number: Parser[Number] =
    wholeNumber ^^ (n => Number(Integer.parseInt(n)))

  override def safeWithoutParens: Parser[Term] =
    number | super.safeWithoutParens

  override def singleTerm: Parser[Term] = number | super.singleTerm

  def plus: Parser[Term]  = rep1sep(singleTerm, "+") ^^ (xs => xs.reduceLeft(Plus))

  override def term: Parser[Term] = plus ||| super.term
}

trait ProductParsers extends LambdaCalculusParsers {
  self: LambdaCalculusParsers =>

  type CalculusType <: LambdaCalculus with Products
  import calculus._

  def pi1: Parser[Pi1] =
    "pi1" ~> singleTerm ^^ Pi1

  def pi2: Parser[Pi2] =
    "pi2" ~> singleTerm ^^ Pi2

  def product: Parser[Term] =
    "(" ~> term ~ ("," ~>! term <~ ")") ^^ { case l ~ r => Product(l, r) }

  override def safeWithoutParens: Parser[Term] =
    product | super.safeWithoutParens

  override def singleTerm: Parser[Term] =
    product | pi1 | pi2 | super.singleTerm
}

trait LetBindingParsers extends LambdaCalculusParsers {
  self: LambdaCalculusParsers =>

  type CalculusType <: LambdaCalculus with LetBindings
  import calculus._

  override def variable: Parser[Var] =
    not("let" | "in") ~> super.variable

  def pattern: Parser[Pattern] =
    variable ^^ VarPattern

  def let: Parser[Let] =
    "let" ~> pattern ~ ("=" ~>! term ~! ("in" ~>! term)) ^^ { case p ~ (b ~ i) => Let(p, b, i) }

  override def singleTerm: Parser[Term] = let | super.singleTerm
}

trait ProductPatternBindingParsers extends LetBindingParsers {
  self: LambdaCalculusParsers with LetBindingParsers =>

  type CalculusType <: LambdaCalculus with LetBindings with ProductPatterns
  import calculus._

  def productPattern: Parser[ProductPattern] =
    "(" ~> pattern ~ ("," ~>! pattern <~ ")") ^^ {case lp ~ rp => ProductPattern(lp, rp) }

  override def pattern: Parser[Pattern] = productPattern | super.pattern
}

trait CommentParsers extends LambdaCalculusParsers {
  self: LambdaCalculusParsers =>

  override protected val whiteSpace =
    """(\s|//.*\n)+""".r

}

trait FullCalculusParsers
  extends LambdaCalculusParsers
  with RecordParsers
  with MergeParsers
  with ProductParsers
  with PrimitiveArithmeticParsers
  with LetBindingParsers
  with ProductPatternBindingParsers
  with CommentParsers {

  type CalculusType = FullCalculus
}

