package record_calculus

trait LambdaCalculus {
  sealed trait Term
  case class Var(name: String) extends Term
  case class Lambda(v: Var, body: Term) extends Term
  case class App(m: Term, n: Term) extends Term

  def subst(v: Var, by: Term)(in: Term): Term =
    in match {
      case _: v.type => by
      case Var(_) => in
      case Lambda(_: v.type, body) => in
      case Lambda(w, body) => Lambda(w, subst(v, by)(body))
      case App(m, n) => App(subst(v, by)(m), subst(v, by)(n))
    }

  def whnfEval(term: Term, rhss: Seq[Term]): Term =
    (term, rhss) match {
      case (App(m, n), _) => whnfEval(m, n +: rhss)
      case (Lambda(v, b), rhs :: rhss) => whnfEval(subst(v, rhs)(b), rhss)
      case _ => rhss.foldLeft(term)(App)
    }

  def eval(term: Term): Term = whnfEval(fixVars(term)._2, Seq())

  def fixVars(term: Term, env: Map[String, Var] = Map()): (Map[String, Var], Term) =
    term match {
      case Var(x) if env.contains(x) => (env, env(x))
      case v@Var(y) => (env + (y -> v), term)
      case Lambda(v@(Var(x)), body) =>
        val oldx = env.get(x)
        val (envBody, newBody) = fixVars(body, env + (x -> v))
        val envBodyClean = envBody - x
        val newEnv =
          oldx match {
            case Some(oldV) => envBodyClean + (x -> oldV)
            case None => envBodyClean
          }
        (newEnv, Lambda(v, newBody))
      case App(m, n) =>
        val (envM, newM) = fixVars(m, env)
        val (envN, newN) = fixVars(n, envM)
        (envN, App(newM, newN))
    }
}

trait Records extends LambdaCalculus { self: LambdaCalculus =>
  case class Record(labels: Map[String, Term]) extends Term
  case class Get(m: Term, label: String) extends Term

  override def subst(v: Var, by: Term)(in: Term): Term =
    in match {
      case Record(labels) => Record(labels.mapValues(subst(v, by)))
      case Get(m, label) => Get(subst(v, by)(m), label)
      case _ => super.subst(v, by)(in)
    }

  override def whnfEval(term: Term, rhss: Seq[Term]): Term =
    (term, rhss) match {
      case (Get(m, label), rhss) =>
        whnfEval(m, Seq()) match {
          case Record(labels) if labels.contains(label) =>
            whnfEval(labels(label), rhss)
          case mEv => rhss.foldLeft(Get(mEv, label) : Term)(App)
        }
      case _ => super.whnfEval(term, rhss)
    }

  override def fixVars(term: Term, env: Map[String, Var] = Map()): (Map[String, Var], Term) =
    term match {
      case Record(labels) =>
        val (newEnv, newLabels) =
          labels.foldLeft((env, Map[String, Term]()))({
            case ((env, newLabels), (l, e)) =>
              val (env2, e2) = fixVars(e, env)
              (env2, newLabels + (l -> e2))
            })
        (newEnv, Record(newLabels))
      case Get(m, label) =>
        val (e1, newM) = fixVars(m, env)
        (e1, Get(newM, label))
      case _ => super.fixVars(term, env)
    }
}

trait Merge extends Records { self: LambdaCalculus with Records =>
  case class Merge(m: Term, r: Record) extends Term

  override def subst(v: Var, by: Term)(in: Term): Term =
    in match {
      case Merge(m, Record(labels)) => Merge(subst(v, by)(m), Record(labels.mapValues(subst(v, by))))
      case _ => super.subst(v, by)(in)
    }

  override def whnfEval(term: Term, rhss: Seq[Term]): Term =
    (term, rhss) match {
      case (Merge(m, Record(newLabels)), _) =>
        whnfEval(m, Seq()) match {
          case Record(oldLabels) => whnfEval(Record(oldLabels ++ newLabels), rhss)
          case mEv => rhss.foldLeft(Merge(mEv, Record(newLabels)) : Term)(App)
        }
      case _ => super.whnfEval(term, rhss)
    }

  override def fixVars(term: Term, env: Map[String, Var]): (Map[String, Var], Term) =
    term match {
      case Merge(m, r) =>
        val (envM, mNew) = fixVars(m, env)
        val (envR, rNew@Record(_)) = fixVars(r, envM)
        (envR, Merge(mNew, rNew))
      case _ => super.fixVars(term, env)
    }

  def test = {
    val x = Var("x")
    val y = Var("y")
    App(Get(Merge(Record(Map()), Record(Map("l1" -> Lambda(x, App(x, x))))), "l1"), y)
  }
}

trait PrimitiveArithmetic extends LambdaCalculus { self: LambdaCalculus =>
  case class Number(n: Int) extends Term
  case class Plus(x: Term, y: Term) extends Term

  override def subst(v: Var, by: Term)(in: Term): Term =
    in match {
      case Number(_) => in
      case Plus(x, y) => Plus(subst(v, by)(x), subst(v, by)(y))
      case _ => super.subst(v, by)(in)
    }

  override def whnfEval(term: Term, rhss: Seq[Term]): Term =
    term match {
      case Plus(x, y) =>
        whnfEval(x, Seq()) match {
          case Number(n1) =>
            whnfEval(y, Seq()) match {
              case Number(n2) => whnfEval(Number(n1 + n2), rhss)
              case yN => rhss.foldLeft(Plus(Number(n1), yN) : Term)(App)
            }
          case xN => rhss.foldLeft(Plus(xN, y) : Term)(App)
        }
      case _ => super.whnfEval(term, rhss)
    }

  override def fixVars(term: Term, env: Map[String, Var]): (Map[String, Var], Term) =
    term match {
      case Number(_) => (env, term)
      case Plus(l, r) =>
        val (envL, lNew) = fixVars(l, env)
        val (envR, rNew) = fixVars(r, envL)
        (envR, Plus(lNew, rNew))
      case _ => super.fixVars(term, env)
    }

  def numberTest = {
    val x = Var("x")
    val y = Var("y")
    val z = Var("z")
    App(App(Lambda(x, Lambda(y, Plus(Plus(x, x), App(y, Number(3))))), Number(1)), Lambda(z, Plus(z, z)))
  }
}


trait Products extends LambdaCalculus { self: LambdaCalculus =>
  case class Product(left: Term, right: Term) extends Term
  case class Pi1(term: Term) extends Term
  case class Pi2(term: Term) extends Term

  override def subst(v: Var, by: Term)(in: Term): Term =
    in match {
      case Product(l, r) => Product(subst(v, by)(l), subst(v, by)(r))
      case Pi1(t) => Pi1(subst(v, by)(t))
      case Pi2(t) => Pi2(subst(v, by)(t))
      case _ => super.subst(v, by)(in)
    }

  override def whnfEval(term: Term, rhss: Seq[Term]): Term =
    term match {
      case Pi1(t) =>
        whnfEval(t, Seq()) match {
          case Product(l, _) => whnfEval(l, rhss)
          case tN => rhss.foldLeft(Pi1(tN) : Term)(App)
        }
      case Pi2(t) =>
        whnfEval(t, Seq()) match {
          case Product(_, r) => whnfEval(r, rhss)
          case tN => rhss.foldLeft(Pi2(tN) : Term)(App)
        }
      case _ => super.whnfEval(term, rhss)
    }

  override def fixVars(term: Term, env: Map[String, Var]): (Map[String, Var], Term) =
    term match {
      case Product(l, r) =>
        val (envL, lNew) = fixVars(l, env)
        val (envR, rNew) = fixVars(r, envL)
        (envR, Product(lNew, rNew))
      case Pi1(t) =>
        val (envNew, tNew) = fixVars(t, env)
        (envNew, Pi1(tNew))
      case Pi2(t) =>
        val (envNew, tNew) = fixVars(t, env)
        (envNew, Pi2(tNew))
      case _ => super.fixVars(term, env)
    }
  def productTest = {
    App(Pi1(Product(Lambda(Var("x"), App(Var("x"), Var("x"))), Var("y"))),
        Pi2(App(Lambda(Var("x"), Var("x")),
                Product(Var("y"), Lambda(Var("z"), Var("z"))))))
  }
}

trait LetBindings extends LambdaCalculus { self: LambdaCalculus =>
  sealed trait Pattern
  case class VarPattern(v: Var) extends Pattern

  case class Let(p: Pattern, binding: Term, in: Term) extends Term

  def boundSomewhere(p: Pattern, v: Var): Boolean =
    vars(p).contains(v.name)

  def vars(p: Pattern): Map[String, Var] =
    p match {
      case VarPattern(v@Var(varName)) => Map(varName -> v)
      case _ => Map()
    }

  def bind(p: Pattern, binding: Term): Term => Term =
    p match {
      case VarPattern(w) => subst(w, binding)
      case _ => (x => x)
    }

  override def subst(v: Var, by: Term)(in: Term): Term =
    in match {
      case Let(p, binding, inTerm) =>
        if (boundSomewhere(p, v)) {
          Let(p, subst(v, by)(binding), inTerm)
        } else {
          Let(p, subst(v, by)(binding), subst(v, by)(inTerm))
        }
      case _ => super.subst(v, by)(in)
    }

  override def whnfEval(term: Term, rhss: Seq[Term]): Term =
    term match {
      case Let(p, binding, in) => whnfEval(bind(p, binding)(in), rhss)
      case _ => super.whnfEval(term, rhss)
    }

  override def fixVars(term: Term, env: Map[String, Var]): (Map[String, Var], Term) =
    term match {
      case Let(pattern, binding, in) =>
        val patternVars = vars(pattern)
        val (bindEnv, bindingNew) = fixVars(binding, env)
        val (inEnv, inNew) = fixVars(in, bindEnv ++ patternVars)
        val inEnvClean = inEnv -- patternVars.keys
        val newEnv =
          patternVars.keys.foldLeft(inEnvClean)({
              case (s, varName) =>
                bindEnv.get(varName) match {
                  case Some(oldV) => s + (varName -> oldV)
                  case None => s
                }
            })
        (newEnv, Let(pattern, bindingNew, inNew))
      case _ => super.fixVars(term, env)
    }

  def letTest = {
    App(App(Lambda(Var("x"), Lambda(Var("z"), Let(VarPattern(Var("x")), Var("x"), App(Var("z"), Var("x"))))), Var("y")), Var("z"))
  }
}


trait ProductPatterns extends LambdaCalculus with Products with LetBindings {
  self : LambdaCalculus with Products with LetBindings =>

  case class ProductPattern(l: Pattern, r: Pattern) extends Pattern

  override def vars(p: Pattern): Map[String, Var] =
    p match {
      case ProductPattern(l, r) => vars(l) ++ vars(r)
      case _ => super.vars(p)
    }

  override def bind(p: Pattern, binding: Term): Term => Term =
    p match {
      case ProductPattern(lp, rp) =>
        whnfEval(binding, Seq()) match {
          case Product(l, r) => bind(lp, l) compose bind(rp, r)
          case _ => sys.error(s"Cannot match product pattern: $binding")
        }
      case _ => super.bind(p, binding)
    }

  def productPatternTest = {
    Let(ProductPattern(VarPattern(Var("x")), VarPattern(Var("y"))),
      App(Lambda(Var("x"), Var("x")), Product(Var("y"),
      Var("x"))), Product(Var("x"), Var("y")))
  }
}

trait FullCalculus
  extends LambdaCalculus
  with Records
  with Merge
  with Products
  with PrimitiveArithmetic
  with LetBindings
  with ProductPatterns {
}


