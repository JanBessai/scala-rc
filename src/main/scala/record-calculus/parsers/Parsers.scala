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

object LambdaCalculusParsers {
  trait Memo extends LambdaCalculusParsers with PackratParsers {
    self: parsers.LambdaCalculusParsers =>
    import calculus._

    override lazy val variable: PackratParser[Var] = super.variable
    override lazy val lambda: PackratParser[Lambda] = super.lambda
    override lazy val singleTerm: PackratParser[Term] = super.singleTerm
    override lazy val safeWithoutParens: PackratParser[Term] = super.safeWithoutParens
    override lazy val term: PackratParser[Term] = super.term
  }
}


trait RecordParsers extends LambdaCalculusParsers {
  self: LambdaCalculusParsers =>

  type CalculusType <: LambdaCalculus with Records
  import calculus._

  def member: Parser[(String, Term)] =
    ident ~! ("=" ~>! term) ^^ { case l ~ m => (l, m) }

  def record: Parser[Record] =
    "{" ~>! repsep(member, ",") <~! "}" ^^ (labels => Record(Map() ++ labels))

  override def safeWithoutParens: Parser[Term] =
    record | super.safeWithoutParens

  def get: Parser[Get] =
    ("(" ~> term <~ ")" | safeWithoutParens) ~ ("." ~>! ident) ^^ { case t ~ l => Get(t, l) }

  override def singleTerm: Parser[Term] =
    get | record | super.singleTerm
}

object RecordParsers {
  trait Memo extends RecordParsers with PackratParsers {
    self: LambdaCalculusParsers with RecordParsers with LambdaCalculusParsers.Memo =>
    import calculus._

    override lazy val member: PackratParser[(String, Term)] = super.member
    override lazy val record: PackratParser[Record] = super.record
    override lazy val get: PackratParser[Get] = super.get
  }

}


trait MergeParsers extends RecordParsers {
  self: LambdaCalculusParsers with RecordParsers =>

  type CalculusType <: LambdaCalculus with Records with Merge
  import calculus._

  def merge: Parser[Merge] =
    ("(" ~> term <~ ")" | safeWithoutParens) ~ ("<+>" ~>! record) ^^ { case m ~ r => Merge(m, r) }

  override def singleTerm: Parser[Term] = merge | super.singleTerm
}

object MergeParsers {
  trait Memo extends MergeParsers with PackratParsers {
    self: LambdaCalculusParsers with RecordParsers with LambdaCalculusParsers.Memo with RecordParsers.Memo =>
    import calculus._
    override lazy val merge: PackratParser[Merge] = super.merge
  }
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

object PrimitiveArithmeticParsers {
  trait Memo extends PrimitiveArithmeticParsers with PackratParsers {
    self: LambdaCalculusParsers with PrimitiveArithmeticParsers with LambdaCalculusParsers.Memo =>
    import calculus._
    override lazy val number: PackratParser[Number] = super.number
    override lazy val plus: PackratParser[Term] = super.plus
  }
}

trait ProductParsers extends LambdaCalculusParsers {
  self: LambdaCalculusParsers =>

  type CalculusType <: LambdaCalculus with Products
  import calculus._

  def pi1: Parser[Pi1] =
    "pi1" ~> singleTerm ^^ Pi1

  def pi2: Parser[Pi2] =
    "pi2" ~> singleTerm ^^ Pi2

  def product: Parser[Product] =
    "(" ~> term ~ ("," ~>! term <~ ")") ^^ { case l ~ r => Product(l, r) }

  override def safeWithoutParens: Parser[Term] =
    product | super.safeWithoutParens

  override def singleTerm: Parser[Term] =
    product | pi1 | pi2 | super.singleTerm
}

object ProductParsers {
  trait Memo extends ProductParsers with PackratParsers {
    self: LambdaCalculusParsers with ProductParsers with LambdaCalculusParsers.Memo =>
    import calculus._
    override lazy val pi1: PackratParser[Pi1] = super.pi1
    override lazy val pi2: PackratParser[Pi2] = super.pi2
    override lazy val product: PackratParser[Product] = super.product
  }
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

object LetBindingParsers {
  trait Memo extends LetBindingParsers with PackratParsers {
    self: LambdaCalculusParsers with LetBindingParsers with LambdaCalculusParsers.Memo =>
    import calculus._
    override lazy val pattern: PackratParser[Pattern] = super.pattern
    override lazy val let: PackratParser[Let] = super.let
  }
}

trait ProductPatternBindingParsers extends LetBindingParsers {
  self: LambdaCalculusParsers with LetBindingParsers =>

  type CalculusType <: LambdaCalculus with LetBindings with ProductPatterns
  import calculus._

  def productPattern: Parser[ProductPattern] =
    "(" ~> pattern ~ ("," ~>! pattern <~ ")") ^^ {case lp ~ rp => ProductPattern(lp, rp) }

  override def pattern: Parser[Pattern] = productPattern | super.pattern
}

object ProductPatternBindingParsers {
  trait Memo extends ProductPatternBindingParsers with PackratParsers {
    self: LambdaCalculusParsers with LetBindingParsers with ProductPatternBindingParsers with LambdaCalculusParsers.Memo =>
    import calculus._
    override lazy val productPattern: PackratParser[ProductPattern] = super.productPattern
  }
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
  with CommentParsers
  with LambdaCalculusParsers.Memo
  with RecordParsers.Memo
  with MergeParsers.Memo
  with PrimitiveArithmeticParsers.Memo
  with LetBindingParsers.Memo
  with ProductPatternBindingParsers.Memo {

  type CalculusType = FullCalculus
}

