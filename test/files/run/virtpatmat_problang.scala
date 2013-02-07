trait ProbCore {
  type Prob = Double
  case class RandVar[+A](val dist: List[(A,Prob)]) {
    def flatMap[B](f: A => RandVar[B]): RandVar[B] =
      RandVar(dist.flatMap{case (x,p) => f(x).factor(p).dist})
    def map[B](f: A => B): RandVar[B] =
      RandVar(dist.map{case (x,p) => (f(x),p)})
    // NOTE(namin): orElse is overcounting if the cases are not disjoint! See roulete payoffs.
    // How can we fix this? And do we want to?
    // P(A \/ B) = P(A) + P(B) - P(A /\ B) <-- can we get P(A /\ B)?
    def orElse[B >: A](that: RandVar[B]) =
      RandVar(this.dist ++ that.dist)
    def weight =
      dist.map(_._2).sum
    def factor(w: Double): RandVar[A] =
      RandVar(dist.map{case (x,p) => (x,p*w)})
    def consolidate: RandVar[A] = {
      RandVar(dist.filter(_._2 > 0).groupBy(_._1).map{case (x,ps) => (x, ps.map(_._2).sum)}.toList.sortBy{case (x,p) => (-p,x.toString)})
    }
    def normalize: RandVar[A] = {
      val r = flatten.consolidate
      r.factor(r.weight)
    }
    def flatten: RandVar[A] =
      RandVar(dist.flatMap{case (x,p) => x match {
	  case r @ RandVar(_) => r.flatten.factor(p).dist
	  case _ => List((x,p))
      }}).asInstanceOf[RandVar[A]]
  }

  def boolFlip(p: Double) = RandVar(List((true, p), (false, 1-p)))
  def always[A](x: A) = RandVar(List((x, 1.0)))
  val never = RandVar(Nil)

  def weightedCases[A](inp: (A,Prob)*) = RandVar(inp.toList)
  def countedCases[A](inp: (A, Int)*) = {
    val total = 1.0*inp.map(_._2).sum
    weightedCases(inp map { case (x,c) => (x,c/total) }:_*)
  }
}

trait ProbLift extends ProbCore {
  import scala.language.implicitConversions
  implicit def liftRandVar[T](x: T): RandVar[T] = always(x)
}

trait ProbCond extends ProbCore with EmbeddedControls {
  def __ifThenElse[T](cond: => RandVar[Boolean], thenp: => RandVar[T], elsep: => RandVar[T]): RandVar[T] =
    cond.flatMap(c => c match {
      case true => thenp
      case false => elsep
    })
}

trait ProbMatcher extends ProbCore with ProbLift {
  object __match {
    def one[T](x: T): RandVar[_] =
      if (x.isInstanceOf[RandVar[_]]) x else always(x)
    def zero = never
    def guard[T](cond: Boolean, result: => T): RandVar[_] =
      if (cond) one(result) else zero
    def runOrElse[T, U](in: T)(matcher: T => RandVar[U]): RandVar[U] =
      matcher(in).flatten
  }
}

trait ProbExtractor extends ProbMatcher {
  trait ValueExtractor {
    type A
    val value: A
    def unapply(that: A): RandVar[A] = if (value == that) always(value) else never
    def unapply(r: RandVar[A]): RandVar[A] = r.flatMap(unapply)
  }
  class CaseExtractor(desc: String) extends ValueExtractor {
    type A >: this.type
    val value: A = this
    override def toString = desc
  }
  def ValueExtractor[T](x: T) = new ValueExtractor {
    type A = T
    val value: A = x
  }
  val True = ValueExtractor[Boolean](true)
  val False = ValueExtractor[Boolean](false)
}

trait ProbLang extends ProbCore with ProbCond with ProbMatcher with ProbExtractor

trait ProbPrettyPrint extends ProbCore {
  def pp[A](r: RandVar[A]) = r.dist.map{case (x,p) => x + " : " + p}.mkString("\n")
  def show[A](r: RandVar[A], desc: String = "") = {
    println(desc)
    println(pp(r.normalize))
    println("")
  }
}

trait ProbCondEx extends ProbCond with ProbLift {
  val cond1 = {
    val x = boolFlip(0.5)
    val y = if (x) 1 else if (x) 2 else 3
    y
  }
}

trait ProbMatcherExRoulette extends ProbMatcher with ProbExtractor {
  object RouletteModel {
    sealed class Outcome(name: String) extends CaseExtractor(name) { type A = Outcome }
    object Even extends Outcome("Even")
    object Odd extends Outcome("Odd")
    object Zero extends Outcome("Zero")
  }
  import RouletteModel._

  val roulette = countedCases(Even -> 18, Odd -> 18, Zero -> 1)

  val roulette1 = roulette match {
    case r => r
  }

  val roulettePayoff = roulette match {
    case Even(_) => 10.0
    case Odd(_) => 0.0
    case Zero(_) => 0.0
  }

  val roulettePayoff1 = roulette match {
    case r => r match {
      case Even(_) => 10.0
      case Odd(_) => 0.0
      case Zero(_) => 0.0
    }
  }

  val roulettePayoff2 = roulette match {
    case Even(_) => 10.0
    case _ => 0.0
  }
  val roulettePayoff3 = roulette match {
    case Even(_) => 10.0
    case Odd(_) => 1.0
    case Zero(_) => 1.0
  }
  val roulettePayoff4 = roulette match {
    case Even(_) => 10.0
    case _ => 1.0
  }
  val roulettePayoff5 = roulette match {
    case Even(r) => 10.0
    case Odd(r) => 5.0
    case Zero(r) => 0.0
  }
}

trait ProbLangExRoulette extends ProbLang with ProbMatcherExRoulette {
  import RouletteModel._

  val roulettePayoff6 = roulette match {
    case Even(_) => boolFlip(0.5) match {
      case True(_) => 10.0
      case False(_) => 0.0
    }
    case Odd(_) => 0.0
    case Zero(_) => 0.0
  }

  val roulettePayoff7 = roulette match {
    case Even(_) => 10.0
  }
}

trait ProbLangExTraffic extends ProbLang {
  // TODO
}

object Test extends App with ProbPrettyPrint with ProbCondEx with ProbMatcherExRoulette with ProbLangExRoulette {
  show(cond1, "cond1")
  show(roulette, "roulette")
  show(roulette1, "roulette1")
  show(roulettePayoff, "roulettePayoff")
  show(roulettePayoff1, "roulettePayoff1")
  show(roulettePayoff2, "roulettePayoff2")
  show(roulettePayoff3, "roulettePayoff3")
  show(roulettePayoff4, "roulettePayoff4")
  show(roulettePayoff5, "roulettePayoff5")
  show(roulettePayoff6, "roulettePayoff6")
  show(roulettePayoff7, "roulettePayoff7")
}
