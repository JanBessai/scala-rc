package record_calculus

import scala.language.implicitConversions

//trait BCD {
//  val calculus: LambdaCalculus
//
//  sealed trait Type
//
//  case class Variable(name: String) extends Type
//  sealed trait Arrow extends Type {
//    val source: Type
//    val target: Type
//  }
//  object Arrow {
//    def apply(src: Type, tgt: Type): Arrow { val source: src.type; val target: tgt.type } =
//      new Arrow { val source: src.type = src; val target: tgt.type = tgt }
//    def unapply(arrow: Arrow): Option[(arrow.source.type, arrow.target.type)] =
//      Some((arrow.source, arrow.target))
//  }
//
//  sealed trait Intersection extends Type{
//    val sigma: Type
//    val tau: Type
//  }
//  object Intersection {
//    def apply(s: Type, t: Type): Intersection { val sigma: s.type; val tau: t.type } =
//      new Intersection { val sigma: s.type = s; val tau: t.type = t }
//    def unapply(inter: Intersection): Option[(inter.sigma.type, inter.tau.type)] =
//      Some((inter.sigma, inter.tau))
//  }
//
//  /*object Arrow {
//    def apply(src: Type, tgt: Type): Arrow { val source: src.type; val target: tgt.type } =
//      new Arrow(src, tgt) { val source: src.type = src; val target: tgt.type = tgt }
//  }*/
//
//  /*
//  def removeOuterIntersection(t: Type): Seq[Type] =
//    t match {
//      case Intersection(s, t, types) => s +: t +: types
//      case _ => Seq(t)
//    }
//
//  def removeNestedIntersections(t: Type): Type = {
//    t match {
//      case Arrow(s, t) => Arrow(removeNestedIntersections(s), removeNestedIntersections(t))
//      case Intersection(s, t, ts) =>
//        (removeOuterIntersection(removeNestedIntersections(s))
//         ++ removeOuterIntersection(removeNestedIntersections(t))
//         ++ (ts.flatMap(t => removeOuterIntersection(removeNestedIntersections(t))))) match {
//          case s::t::ts => Intersection(s, t, ts)
//          case s::Seq() => s
//        }
//      case _ => t
//    }
//  }
//  */
//
//  sealed trait SubtypeProof {
//    val subType: Type
//    val superType: Type
//  }
//
//  object InterLessSigma {
//    def apply(i: Intersection):
//      SubtypeProof {
//        val subType: i.type
//        val superType: i.sigma.type
//      } =
//      new SubtypeProof {
//        val subType: i.type = i
//        val superType: i.sigma.type = i.sigma
//      }
//
//    def unapply(p: SubtypeProof): Option[Intersection] =
//      p.subType match {
//        case i: Intersection =>
//          p.superType match {
//            case _: i.sigma.type => Some(i)
//            case _ => None
//          }
//        case _ => None
//      }
//  }
//  object InterLessTau {
//    def apply(i: Intersection):
//      SubtypeProof {
//        val subType: i.type
//        val superType: i.tau.type
//      } =
//      new SubtypeProof {
//        val subType: i.type = i
//        val superType: i.tau.type = i.tau
//      }
//
//    def unapply(p: SubtypeProof): Option[Intersection] =
//      p.subType match {
//        case i: Intersection =>
//          p.superType match {
//            case _: i.tau.type => Some(i)
//            case _ => None
//          }
//        case _ => None
//      }
//  }
//
//  object InterGreater {
//    def apply(s: Type)
//      (sInter: Intersection {
//          val sigma: s.type
//          val tau: s.type
//        } = Intersection(s, s)): SubtypeProof { val subType: s.type; val superType: sInter.type } =
//      new SubtypeProof {
//        val subType: s.type = s
//        val superType: sInter.type = sInter
//      }
//
//    def unapply(ig: SubtypeProof): Option[Type] =
//      ig.superType match {
//        case i@Intersection(_, _) =>
//          (i.sigma, i.tau) match {
//            case (_: ig.subType.type, _: ig.subType.type) => Some(i.sigma)
//            case _ => None
//          }
//        case _ => None
//      }
//  }
//
//  object ArrowInter {
//    def apply(arrowInter: Intersection {
//        val sigma: Arrow
//        val tau: Arrow { val source: sigma.source.type }
//      })
//      (arrow: Arrow {
//          val source: arrowInter.sigma.source.type
//          val target: Intersection {
//            val sigma: arrowInter.sigma.target.type
//            val tau: arrowInter.tau.target.type
//          }
//        } = Arrow(arrowInter.sigma.source,
//              Intersection(arrowInter.sigma.target, arrowInter.tau.target))):
//        SubtypeProof { val subType: arrowInter.type; val superType: arrow.type } =
//      new SubtypeProof {
//        val subType: arrowInter.type = arrowInter
//        val superType: arrow.type = arrow
//      }
//
//    def unapply(p: SubtypeProof):
//      Option[Intersection {
//        val sigma: Arrow
//        val tau: Arrow { val source: sigma.source.type }
//      }] =
//      for {
//        i@Intersection(_, _) <- Some(p.subType)
//        arr1@Arrow(_, _) <- Some(i.sigma)
//        arr2@Arrow(_, _) <- Some(i.tau)
//        __ <- arr2.source match {
//          case _: arr1.source.type => Some(())
//          case _ => None
//        }
//      } yield i.asInstanceOf[Intersection { val sigma: Arrow; val tau: Arrow { val source: sigma.source.type }}]
//
//  }
//
//  sealed trait CompoundSubtypeProof extends SubtypeProof {
//    val p1: SubtypeProof
//    val p2: SubtypeProof
//  }
//
//  object SubtypeInter {
//    def apply(ss: SubtypeProof, ts: SubtypeProof)
//      (subTy: Intersection { val sigma: ss.subType.type; val tau: ts.subType.type } =
//        Intersection(ss.subType, ts.subType),
//       superTy: Intersection { val sigma: ss.superType.type; val tau: ts.superType.type } =
//        Intersection(ss.superType, ts.superType)):
//      CompoundSubtypeProof {
//        val p1: ss.type
//        val p2: ts.type
//        val subType: subTy.type
//        val superType: superTy.type
//      } =
//      new CompoundSubtypeProof {
//        val p1: ss.type = ss
//        val p2: ts.type = ts
//        val subType: subTy.type = subTy
//        val superType: superTy.type = superTy
//      }
//
//    def unapply(p: SubtypeProof): Option[(SubtypeProof, SubtypeProof)] =
//      p match {
//        case cp: CompoundSubtypeProof =>
//          for {
//            i1@Intersection(_, _) <- Some(cp.subType)
//            i2@Intersection(_, _) <- Some(cp.subType)
//            _ <- (i1.sigma, i2.sigma) match {
//              case (_: cp.p1.subType.type, _: cp.p1.superType.type) => Some(())
//              case _ => None
//            }
//            _ <- (i1.tau, i2.tau) match {
//              case (_: cp.p2.subType.type, _: cp.p2.superType.type) => Some(())
//              case _ => None
//            }
//          } yield (cp.p1, cp.p2)
//        case _ => None
//      }
//  }
//
//  object CoContra {
//    def apply(ss: SubtypeProof, ts: SubtypeProof)
//      (subTy: Arrow { val source: ss.superType.type; val target: ts.subType.type } =
//        Arrow(ss.superType, ts.subType),
//       superTy: Arrow { val source: ss.subType.type; val target: ts.superType.type } =
//        Arrow(ss.subType, ts.superType)):
//      CompoundSubtypeProof {
//        val p1: ss.type
//        val p2: ts.type
//        val subType: subTy.type
//        val superType: superTy.type
//      } =
//      new CompoundSubtypeProof {
//        val p1: ss.type = ss
//        val p2: ts.type = ts
//        val subType: subTy.type = subTy
//        val superType: superTy.type = superTy
//      }
//
//    def unapply(p: SubtypeProof): Option[(SubtypeProof, SubtypeProof)] =
//      p match {
//        case cp: CompoundSubtypeProof =>
//          for {
//            arr1@Arrow(_, _) <- Some(cp.subType)
//            arr2@Arrow(_, _) <- Some(cp.superType)
//            _ <- (arr1.source, arr1.target) match {
//              case (_: cp.p1.superType.type, _: cp.p2.subType.type) => Some(())
//              case _ => None
//            }
//            _ <- (arr2.source, arr2.target) match {
//              case (_: cp.p1.subType.type, _: cp.p1.superType.type) => Some(())
//              case _ => None
//            }
//          } yield (cp.p1, cp.p2)
//        case _ => None
//      }
//  }
//  object Transitivity {
//    def apply(l: SubtypeProof)(r: SubtypeProof { val subType: l.superType.type }):
//      CompoundSubtypeProof {
//        val p1: l.type
//        val p2: r.type
//        val subType: l.subType.type
//        val superType: r.superType.type
//      } =
//      new CompoundSubtypeProof {
//        val p1: l.type = l
//        val p2: r.type = r
//        val subType: l.subType.type  = l.subType
//        val superType: r.superType.type = r.superType
//      }
//
//    trait Sigma {
//      import scala.language.higherKinds
//      type X
//      val exists: X
//      type P <: { type Holds }
//      val suchThat: P#Holds
//    }
//
//    def unapply(p: SubtypeProof): Option[Sigma {
//        type X = SubtypeProof
//        type P = { type Holds = SubtypeProof { val subType: exists.superType.type } }
//      }] = throw new Exception()
//      p match {
//        case cp: CompoundSubtypeProof =>
//          (cp.subType, cp.p2.subType, cp.superType) match {
//            case (_: cp.p1.subType.type, _: cp.p1.superType.type, _: cp.p2.superType.type) =>
//              Some(new Sigma {
//                  type X = SubtypeProof
//                  type P[evidence] = SubtypeProof { val subType: evidence.superType.type }
//                  val exists = cp.p1
//                  val suchThat = cp.p2.asInstanceOf[SubtypeProof { val subType: evidence.superType.type }]
//                })
//            case _ => None
//          }
//        case _ => None
//      }
//    def unapply(p: CompoundSubtypeProof {
//        val subType: p1.subType.type
//        val superType: p2.superType.type
//      }): Option[(p.p1.type, p.p2.type)] =
//      Some((p.p1, p.p2))
//  }
///*
//  def reflexivity(sigma: Type): SubtypeProof { val subType: sigma.type; val superType: sigma.type } = {
//    val ig = InterGreater(sigma)()
//    val il = InterLessSigma(ig.superType)
//    Transitivity(ig)(il)
//  }
//*/
///*
//  trait TypeEqualityProof {
//    val p1: SubtypeProof
//    val p2: SubtypeProof { val subType: p1.superType.type; val superType: p1.subType.type }
//  }
//
//  object TypeEqualityProof {
//    def apply(lte: SubtypeProof)(gte: SubtypeProof { val subType: lte.superType.type; val superType: lte.subType.type }):
//        TypeEqualityProof { val p1: lte.type; val p2: gte.type } =
//      new TypeEqualityProof { val p1: lte.type = lte; val p2: gte.type = gte }
//    def unapply(teq: TypeEqualityProof): Option[(teq.p1.type, teq.p2.type)] =
//      Some((teq.p1, teq.p2))
//  }
//
//  def targetDistributivity(ai: ArrowInter.AI[_ <: Type, _ <: Type, _ <: Type]): TypeEqualityProof = {
//    val ple: SubtypeProof {
//        val subType: ai.type
//        val superType: Arrow {
//          val source: subType.sigma.source.type
//          val target: Intersection {
//              val sigma: subType.sigma.target.type
//              val tau: subType.tau.target.type
//            }
//        }
//      } = ArrowInter(ai)
//
//    val sigmaRefl: SubtypeProof {
//        val subType: ple.superType.source.type
//        val superType: ple.superType.source.type
//      } = reflexivity(ple.superType.source)
//    val selTau: SubtypeProof {
//        val subType:  ple.superType.target.type
//        val superType: ple.superType.target.sigma.type
//      } = InterLessSigma(ple.superType.target)
//    val selRho: SubtypeProof {
//        val subType: ple.superType.target.type
//        val superType: ple.superType.target.tau.type
//      } = InterLessTau(ple.superType.target)
//
//    val lessLeft: SubtypeProof {
//        val subType: ple.superType.type
//        val superType: ple.subType.sigma.type
//      } = CoContra(sigmaRefl, selTau)
//    val lessRight = CoContra(sigmaRefl, selRho)
//
//    val pgt = SubtypeInter(lessLeft, lessRight)
//
//    //new TypeEqualityProof { val p1 = ple; val p2: SubtypeProof { val subType: ple.superType.type; val superType: ple.subType.type }  = pgt }
//    throw new UnsupportedOperationException()
//  }
//
//*/
//
///*
//  sealed trait Substitution
//  case class Identical() extends Substitution
//  case class Substitute(v: Variable, by: Type) extends Substitution
//
//  implicit def applySubstitution(s: Substitution): Type => Type =
//    s match {
//      case Identical() => (x => x)
//      case Substitute(v, by) => {
//        case _: v.type => by
//        case in@Variable(_) => in
//        case Arrow(src, tgt) =>
//          Arrow(applySubstitution(s)(src), applySubstitution(s)(tgt))
//        case Intersection(t1, t2, ts) =>
//          Intersection(applySubstitution(s)(t1), applySubstitution(s)(t2), tys map applySubstitution(s))
//      }
//    }
//
//  abstract class Environment {
//    def apply(v: calculus.Var): Type
//    def extend(v: calculus.Var, t: Type): Environment = new Environment {
//      def apply(w: calculus.Var): Type =
//        w match {
//          case _: v.type => t
//          case _ => Environment.this.apply(w)
//        }
//    }
//  }
//
//  class EmptyEnvironment extends Environment {
//    def apply(v: calculus.Var): Nothing = sys.error("Type not found")
//  }
//
//  sealed trait Proof {
//    val term: calculus.Term
//    val ty: Type
//  }
//
//  case class Ax(env: Environment, v: calculus.Var, s: Substitution = Identical()) extends Proof {
//    val term = v
//    val ty = s(env(v))
//  }
//*/
//  /*case class ArrEMinor(m: Proof { val ty: Arrow }) {
//    case class ArrE(n: Proof { val ty: m.ty.source.type }) extends Proof {
//      val term = calculus.App(m.term, n.term)
//      val ty = m.ty.target
//    }
//  }*/
//
//}

