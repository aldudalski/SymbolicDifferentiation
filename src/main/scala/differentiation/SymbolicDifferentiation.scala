package differentiation

import java.text.DecimalFormat

object SymbolicDifferentiation {

  trait Expression {
    val args: List[Expression]
    val command: Command

    def eval: Expression

    def differentiate: Expression

    override def toString: String = s"(${command.name}${if (args.length > 0) " " else ""}${args.mkString(" ")})"
  }

  trait Command {
    val name: String

    override def toString: String = name
  }

  abstract class Term(val name: String) extends Expression with Command {
    val command: Term = this
    val eval: Term = this
    val args = List(command)
  }

  case object NAN extends Term("NAN") {
    def differentiate = NAN
  }

  case object NULL extends Term("") {
    def differentiate = NULL
  }

  case class Constant(c: Double) extends Term(if (c.toLong.toDouble == c) c.toLong.toString else c.toString) {
    def differentiate: Expression = Constant(0)
  }

  case class Variable(v: String) extends Term(v) {
    def differentiate: Expression = Constant(1)
  }

  abstract class Op(arg1: Expression, arg2: Expression, val name: String) extends Command with Expression {
    val args = List(arg1, arg2)
    val command = this
  }

  case class Minus(m1: Expression, m2: Expression) extends Op(m1, m2, "-") {
    def eval = Plus(m1, Times(m2, Constant(-1))).eval

    def differentiate = eval.differentiate

    //override val command = this
  }

  case class Plus(t1: Expression, t2: Expression) extends Op(t1, t2, "+") {
    def differentiate = {
      val t1d = t1.eval.differentiate
      val t2d = t2.eval.differentiate
      t1d match {
        case Constant(t) => if (t == 0) t2d else Plus(t1d, t2d).eval
        case _ => Plus(t1d, t2d).eval
      }
    }

    def eval = {
      (t1.eval, t2.eval) match {
        case (Constant(0), a) => a
        case (a, Constant(0)) => a
        case (Constant(a), Constant(b)) => Constant(a + b)
        case (Variable(a), Variable(b)) => if (a == b) Times(Variable(a), Constant(2)) else this
        case (a@Variable(_), b@Constant(_)) => Plus(a, b)
        case (a@Constant(_), b@Variable(_)) => Plus(a, b)
        case (Plus(a, b), Plus(c, d)) => Plus(Plus(a, c).eval, Plus(b, d).eval)
        case (a, b) => if (a == t1 && b == t2) this else Plus(a, b).eval
      }
    }

    //override val command = this
  }

  case class Times(t1: Expression, t2: Expression) extends Op(t1, t2, "*") {
    def differentiate = {
      val fPrime = t1.eval.differentiate
      val gPrime = t2.eval.differentiate
      (t1.eval, t2.eval) match {
        case (Constant(a), Variable(_)) => Constant(a)
        case (Variable(_), Constant(a)) => Constant(a)
        case (f, g) => Plus(Times(f, gPrime).eval, Times(g, fPrime).eval).eval //product rule
      }
    }

    def eval = {
      (t1.eval, t2.eval) match {
        case (Constant(0), _) | (_, Constant(0)) => Constant(0)
        case (Constant(1), a) => a
        case (b, Constant(1)) => b
        case (Constant(a), Constant(b)) => Constant(a * b)
        case (Constant(a), Times(Constant(b), c)) => Times(Constant(a*b), c)
        case (Constant(a), Times(b, Constant(c))) => Times(Constant(a*c), b)
        case (a@Variable(_), b@Variable(_)) => if (a.name == b.name) Power(a, Constant(2)) else Times(a, b)
        case (a, b) => if (a == t1 && b == t2) Times(a, b) else Times(a, b).eval
      }
    }

    //override val command = this
  }

  case class Divide(t1: Expression, t2: Expression) extends Op(t1, t2, "/") {
    def differentiate = {
      val fPrime = t1.eval.differentiate
      val gPrime = t2.eval.differentiate
      (t1.eval, t2.eval) match {
        case (a@Constant(_), b@Variable(_)) => Divide(Times(a, Constant(-1)).eval, Power(b, Constant(2)).eval).eval
        case (Variable(_), b@Constant(_)) => Divide(Constant(1), b).eval
        case (f, g) => Divide(Minus(Times(fPrime, g).eval, Times(f, gPrime).eval).eval, Power(g, Constant(2)).eval).eval //quotient rule
      }
    }

    def eval = {
      (t1.eval, t2.eval) match {
        case (Constant(0), _) => Constant(0)
        case (_, Constant(0)) => NAN
        case (b, Constant(1)) => b
        case (Constant(a), Constant(b)) => Constant(a / b)
        case (a@Variable(_), b@Variable(_)) => if (a.name == b.name) Constant(1) else Divide(a, b)
        case (a, b) => if (a == t1 && b == t2) Divide(a, b) else Divide(a, b).eval
      }
    }

    //override val command = this
  }


  case class Power(t1: Expression, t2: Expression) extends Op(t1, t2, "^") {

    def differentiate = {
      lazy val protoDiff = Times(Times(t2, Power(t1, Minus(t2, Constant(1)).eval).eval).eval, t1.differentiate).eval
      t2.eval match {
        case Constant(t) => if (t == 0) Constant(0) else protoDiff
        case _ => protoDiff
      }
    }

    def eval = {
      (t1.eval, t2.eval) match {
        case (Constant(0), _) => Constant(0)
        case (_, Constant(0)) => Constant(1)
        case (Constant(1), _) => Constant(1)
        case (b, Constant(1)) => b
        case (Constant(a), Constant(b)) => Constant(math.pow(a, b))
        case (a, b) => if (a == t1 && b == t2) Power(a, b) else Power(a, b).eval
      }
    }

    //override val command = this
  }

  abstract class Fn(arg1: Expression, val name: String, constHandler: Double => Double, val cmd: Expression => Fn) extends Command with Expression {
    val args = List(arg1)

    def eval = arg1.eval match {
      case Constant(a) => Constant(constHandler(a))
      case a => cmd(a)
    }

    val command = this
  }

  def ChainRule(f: Expression => Expression, gOfx: Expression) = {
    //f(g(x))	==> f’(g(x))g’(x)
    Times(f(gOfx).differentiate, gOfx.differentiate).differentiate
  }

  case class Sin(t1: Expression) extends Fn(t1, "sin", a => math.sin(a), a => Sin(a)) {
    override def differentiate: Expression = //ChainRule(x => Cos(x), t1)
      Times(t1.eval.differentiate, Cos(t1.eval)).eval

    //override val command: Command = this
  }

  case class Cos(t1: Expression) extends Fn(t1, "cos", a => math.cos(a), a => Cos(a)) {
    override def differentiate: Expression = Times(t1.eval.differentiate, Times(Constant(-1), Sin(t1.eval))).eval

    //override val command: Command = this
  }

  case class Tan(t1: Expression) extends Fn(t1, "tan", a => math.tan(a), a => Tan(a)) {
    override def differentiate: Expression = Times(t1.eval.differentiate, Plus(Constant(1), Power(Tan(t1.eval), Constant(2)))).eval

    //(+ 1 (^ (tan x) 2))
    //Divide(Constant(1), Power(Cos(t1.eval).eval, Constant(2)).eval).eval
    //override val command: Command = this
  }

  case class Exp(t1: Expression) extends Fn(t1, "exp", a => math.pow(math.E, a), a => Exp(a)) {
    override def differentiate: Expression = Times(t1.eval.differentiate, Exp(t1.eval)).eval

    //override val command: Command = this
  }

  case class Ln(t1: Expression) extends Fn(t1, "ln", a => math.log(a), a => Ln(a)) {
    override def differentiate: Expression = Times(t1.eval.differentiate, Divide(Constant(1), t1.eval).eval).eval

    //override val command: Command = this
  }


  object ParseOut {

    def pars(s: String) = {
      println(s"pars $s")

      def parseConstant(s: String): Option[Expression] = {
        val p = """-?[0-9]+\.?[0-9]*""".r
        println(s"parseConst $s")
        if (p.matches(s)) Some(Constant(s.toDouble)) else None
      }

      def parseVariable(s: String): Option[Expression] = {
        val p = """[a-zA-Z]""".r
        println(s"parseVariable $s")
        if (p.matches(s)) Some(Variable(s)) else None
      }

      def parseFn(s: String, t: Expression): Option[Expression] = {
        println(s"parseFn $s")
        s match {
          case "sin" => Some(Sin(t))
          case "cos" => Some(Cos(t))
          case "tan" => Some(Tan(t))
          case "exp" => Some(Exp(t))
          case "ln" => Some(Ln(t))
          case _ => None
        }
      }

      def parseOp(s: String, t1: Expression, t2: Expression): Expression = {
        println(s"parseOp $s")
        s match {
          case "+" => Plus(t1, t2)
          case "-" => Minus(t1, t2)
          case "*" => Times(t1, t2)
          case "/" => Divide(t1, t2)
          case "^" => Power(t1, t2)
          case _ => NULL
        }
      }

      val expression = s.filterNot(i => i == '(' || i == ')').split(" ").reverse

      def process(stack: List[Expression], expression: Array[String]): Expression = {
        if (expression.isEmpty) stack.head
        else {
          val nxt = parseConstant(expression.head)
            .getOrElse(parseVariable(expression.head)
              .getOrElse(parseFn(expression.head, stack.head)
                .getOrElse(parseOp(expression.head, stack.head, stack.tail.head))))
          nxt match {
            case _: Constant | _: Variable => process(nxt :: stack, expression.tail)
            case _: Fn => process(nxt :: stack.tail, expression.tail)
            case _: Op => process(nxt :: stack.tail.tail, expression.tail)
          }
        }
      }

      process(List[Expression](), expression)
    }
  }

}
