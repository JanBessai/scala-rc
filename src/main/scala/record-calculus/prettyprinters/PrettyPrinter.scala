package record_calculus.prettyprinters

import scala.language.implicitConversions

import org.kiama.output.PrettyPrinter

import record_calculus._

trait LambdaCalculusPrettyPrinter extends PrettyPrinter {
  self: LambdaCalculus =>

  def prettyAppLeft(term: Term): Doc =
    term match {
      case Var(_) => term2Doc(term)
      case App(_, _) => term2Doc(term)
      case _ => parens(term2Doc(term))
    }

  def prettyAppRight(term: Term): Doc =
    term match {
      case App(_, _) => parens(term2Doc(term))
      case _ => term2Doc(term)
    }

  implicit def term2Doc(term: Term): Doc =
    term match {
      case Var(name) => text(name)
      case Lambda(v, body) => "\\" <+> group(nest(term2Doc(v) <+> "." </> term2Doc(body), 2))
      case App(m, n) => group(nest(prettyAppLeft(m) <@> prettyAppRight(n), 2))
    }
}

trait RecordPrettyPrinter extends LambdaCalculusPrettyPrinter {
  self: LambdaCalculus with Records with LambdaCalculusPrettyPrinter =>


  override def prettyAppLeft(term: Term): Doc =
    term match {
      case Record(_) => term2Doc(term)
      case Get(_, _) => term2Doc(term)
      case _ => super.prettyAppLeft(term)
    }

  override def prettyAppRight(term: Term): Doc =
    term match {
      case Record(_) => term2Doc(term)
      case _ => super.prettyAppRight(term)
    }

  def prettyRecordEntry(entry: (String, Term)): Doc =
    entry._1 <+> "=" <+> group(nest(term2Doc(entry._2), 2))

  def prettyGetLeft(term: Term): Doc =
    term match {
      case Var(_) => term2Doc(term)
      case Record(_) => term2Doc(term)
      case _ => parens(term2Doc(term))
    }

  override implicit def term2Doc(term: Term): Doc =
    term match {
      case Record(labels) =>
        braces(group(nest(lsep(labels.map(prettyRecordEntry).toList, comma), 2)))
      case Get(m, label) => prettyGetLeft(m) <> "." <> label
      case App(m@Get(_, _), n) => group(nest(prettyAppLeft(m) <> parens(term2Doc(n)), 2))
      case _ => super.term2Doc(term)
    }
}

trait MergePrettyPrinter extends LambdaCalculusPrettyPrinter {
  self: LambdaCalculus with Records with Merge with RecordPrettyPrinter =>

  override implicit def term2Doc(term: Term): Doc =
    term match {
      case Merge(l, r) => group(parens(term2Doc(l)) <+> "<+>" </> nest(term2Doc(r), 2))
      case _ => super.term2Doc(term)
    }
}

trait PrimitiveArithmeticPrettyPrinter extends LambdaCalculusPrettyPrinter {
  self: LambdaCalculus with PrimitiveArithmetic with LambdaCalculusPrettyPrinter =>

  override def prettyAppRight(term: Term): Doc =
    term match {
      case Plus(_, _) => parens(term2Doc(term))
      case _ => super.prettyAppRight(term)
    }

  def prettyPlusOperand(term: Term): Doc =
    term match {
      case Lambda(_, _) => parens(term2Doc(term))
      case App(_, _) => parens(term2Doc(term))
      case _ => term2Doc(term)
    }

  override implicit def term2Doc(term: Term): Doc =
    term match {
      case Number(n) => text(n.toString)
      case Plus(l, r) => group(prettyPlusOperand(l) <+> "+" <+> nest(prettyPlusOperand(r), 2))
      case _ => super.term2Doc(term)
    }
}

trait ProductPrettyPrinter extends LambdaCalculusPrettyPrinter {
  self: LambdaCalculus with Products with LambdaCalculusPrettyPrinter =>

  override def prettyAppLeft(term: Term): Doc =
    term match {
      case Product(_, _) => term2Doc(term)
      case _ => super.prettyAppLeft(term)
    }

  override def prettyAppRight(term: Term): Doc =
    term match {
      case Product(_, _) => term2Doc(term)
      case _ => super.prettyAppRight(term)
    }

  override implicit def term2Doc(term: Term): Doc =
    term match {
      case Product(l, r) => list(List(l, r), "", term2Doc)
      case Pi1(t) => "pi1" <> parens(group(nest(term2Doc(t), 2)))
      case Pi2(t) => "pi2" <> parens(group(nest(term2Doc(t), 2)))
      case _ => super.term2Doc(term)
    }
}

trait LetBindingPrettyPrinter extends LambdaCalculusPrettyPrinter {
  self: LambdaCalculus with LetBindings with LambdaCalculusPrettyPrinter =>

  def prettyPattern(pattern: Pattern): Doc =
    pattern match {
      case VarPattern(v) => term2Doc(v)
    }

  override implicit def term2Doc(term: Term): Doc =
    term match {
      case Let(pattern, binding, in) =>
        "let" <+>
          prettyPattern(pattern) <+> "=" </> group(nest(term2Doc(binding), 2)) <+>
          "in" <@> term2Doc(in)
      case _ => super.term2Doc(term)
    }
}

trait ProductPatternPrettyPrinter extends LetBindingPrettyPrinter {
  self: LambdaCalculus with LetBindings with ProductPatterns with LambdaCalculusPrettyPrinter =>

  override def prettyPattern(pattern: Pattern): Doc =
    pattern match {
      case ProductPattern(l, r) => list(List(l, r), "", prettyPattern)
      case _ => super.prettyPattern(pattern)
    }
}

trait FullPrettyPrinter
  extends LambdaCalculusPrettyPrinter
  with RecordPrettyPrinter
  with MergePrettyPrinter
  with PrimitiveArithmeticPrettyPrinter
  with ProductPrettyPrinter
  with LetBindingPrettyPrinter
  with ProductPatternPrettyPrinter {
  self: LambdaCalculus
    with Records
    with Merge
    with PrimitiveArithmetic
    with Products
    with LetBindings
    with ProductPatterns =>

}
