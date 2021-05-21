import org.junit.{After, Before, Rule, Test}
import differentiation.SymbolicDifferentiation._
import org.junit.rules.TestName

class SymbolicDifferentiationTests {

  val VERBOSE = true
  var _testName: TestName = new TestName

  @Rule
  def testName = _testName
  def testName_=(aTestName: TestName) {_testName = aTestName}

  @Before def printTestCaseNameBefore() {
    if (VERBOSE) println("\n==== START " + testName.getMethodName)
  }

  @After def printTestCaseNameAfter() {
    if (VERBOSE) print("\n---- end   " + testName.getMethodName)
  }

  def test(test: Any, expected: Any) ={
    if (VERBOSE) println(s"\t$test == $expected")
    assert(test ==  expected)
  }

  @Test def ConstantCheck = {
    val a = Constant(5)
    assert(a.name == "5")
    test(a.args.length, 1)
    assert(a.toString == "5")
  }

  @Test def PlusConstantsCheck = {
    val a = Constant(5)
    val b = Constant(10)
    val ans = Plus(a, b)
    assert(ans.args == List(a,b))
    assert(ans.command.toString=="(+ 5 10)")
    assert(ans.name=="+")
    assert(ans.t1==a)
    assert(ans.t2==b)
    assert(ans.toString=="(+ 5 10)")
    assert(ans.differentiate==Constant(0))
    assert(ans.eval==Constant(15))
  }

  @Test def PlusConstMultiCheck = {
    val a = Constant(50)
    val b = Constant(10)
    val c = Constant(100)
    val d = Constant(-7)
    val ans = Plus(a, Plus(b,Plus(c,d)))
    assert(ans.command.toString=="(+ 50 (+ 10 (+ 100 -7)))")
    assert(ans.eval==Constant(153))
  }

  @Test def PlusMultiCheck = {
    val a = Variable("x")
    val b = Constant(10)
    val c = Constant(100)
    val d = Constant(-7)
    val ans = Plus(a, Plus(b,Plus(c,d)))
    assert(ans.command.toString == "(+ x (+ 10 (+ 100 -7)))")
    assert(ans.eval.toString == "(+ x 103)")
    val ans2 = Plus(Plus(b,Plus(c,d)), a)
    assert(ans2.command.toString=="(+ (+ 10 (+ 100 -7)) x)")
    assert(ans2.eval.toString=="(+ 103 x)")
  }

  @Test def MinusCheck = {
    val a = Variable("x")
    val b = Constant(10)
    val c = Constant(100)
    val d = Constant(-7)
    var ans = Minus(a, b)
    assert(ans.command.toString=="(- x 10)")
    assert(ans.eval.toString=="(+ x -10)")
    ans = Minus(b, c)
    assert(ans.command.toString=="(- 10 100)")
    assert(ans.eval.toString=="-90")
    ans = Minus(c, d)
    assert(ans.command.toString=="(- 100 -7)")
    assert(ans.eval.toString=="107")
    ans = Minus(a, Plus(c,d))
    assert(ans.command.toString=="(- x (+ 100 -7))")
    assert(ans.eval.toString=="(+ x -93)")
  }



  @Test def PlusVariableCheck = {
    val a = Variable("x")
    val b = Variable("x")
    val ans = Plus(a, b)
    assert(ans.args == List(a,b))
    assert(ans.command.toString=="(+ x x)")
    assert(ans.command.toString =="(+ x x)")
    assert(ans.eval.toString == "(* x 2)")
    assert(ans.name=="+")
  }

  @Test def PlusMixedCheck = {
    val a = Variable("x")
    val b = Constant(2)
    val ans = Plus(a, b)
    assert(ans.args == List(a,b))
    assert(ans.command.toString =="(+ x 2)")
    assert(ans.eval.toString == "(+ x 2)")
    assert(ans.name=="+")
  }

  @Test def TimesConstantsCheck = {
    val a = Constant(5)
    val b = Constant(10)
    val ans = Times(a, b)
    assert(ans.args == List(a,b))
    assert(ans.command.toString =="(* 5 10)")
    assert(ans.name=="*")
    assert(ans.t1==a)
    assert(ans.t2==b)
    assert(ans.toString=="(* 5 10)")
    assert(ans.differentiate==Constant(0))
    assert(ans.eval==Constant(50))
  }

  @Test def TimesVariableCheck = {
    val a = Variable("x")
    val b = Variable("x")
    val ans = Times(a, b)
    assert(ans.args == List(a,b))
    assert(ans.command.toString =="(* x x)")
    test(ans.eval.toString, "(^ x 2)")// this should be a power check soon
  }

  @Test def TimesMixedCheck = {
    val a = Variable("x")
    val b = Constant(2)
    val ans = Times(a, b)
    assert(ans.args == List(a,b))
    assert(ans.command.toString =="(* x 2)")
    assert(ans.eval.toString == "(* x 2)")
  }

  @Test def DivideCheck = {
    val a = Variable("x")
    val b = Constant(0)
    val c = Constant(1)
    val d = Constant(2)
    test(Divide(c,d).eval.toString, "0.5")
    test(Divide(a,b).eval.toString, "NAN")
    test(Divide(a,c).eval.toString, "x")
    test(Divide(a,d).eval.toString, "(/ x 2)")
    test(Divide(d,a).eval.toString, "(/ 2 x)")

    test(Divide(c,d).eval.differentiate.toString, "0")
    test(Divide(a,b).eval.differentiate.toString, "NAN")
    test(Divide(a,c).eval.differentiate.toString, "1")
    test(Divide(a,d).eval.differentiate.toString, "0.5")
    test(Divide(d,a).eval.differentiate.toString, "(/ -2 (^ x 2))")
  }

  @Test def PowerCheck ={
    val a = Variable("x")
    val b = Constant(0)
    val c = Constant(1)
    val d = Constant(2)
    test(Power(a, d).eval.toString, "(^ x 2)")
    assert(Power(a, b).eval.toString == "1")
    assert(Power(a, c).eval.toString == "x")
    assert(Power(d, d).eval.toString == "4")
    assert(Power(d, c).eval.toString == "2")
    assert(Power(d, b).eval.toString == "1")
  }

  @Test def PowerDiffCheck ={
    val a = Variable("x")
    val b = Constant(0)
    val c = Constant(1)
    val d = Constant(2)
    val e = Constant(3)
    test(Power(a, d).differentiate.toString,"(* 2 x)")
    assert(Power(a, b).differentiate.toString=="0")
    assert(Power(a, c).differentiate.toString=="1")
    assert(Power(a, e).differentiate.toString=="(* 3 (^ x 2))")
  }

  @Test def ExpectedSimplifications = {
    // For example it should not return (* 1 (+ x 1)) but simply the term (+ x 1) similarly it should
    // not return (* 0 (+ x 1)) instead it should return just 0
    test(Times(Constant(1),Plus(Variable("x"), Constant(1))).eval.toString,"(+ x 1)")
    assert(Times(Constant(0),Plus(Variable("x"), Constant(1))).eval.toString=="0")
    // Results with two constant values such as for example (+ 2 2) should be evaluated and returned as a
    // single value 4
    assert(Plus(Constant(2), Constant(2)).eval.toString=="4")
    assert(Times(Constant(2), Constant(2)).eval.toString=="4")
    // Any argument raised to the zero power should return 1 and if raised to 1 should return the same
    //  value or variable. For example (^ x 0) should return 1 and (^ x 1) should return x
    test(Power(Constant(20), Constant(0)).eval, Constant(1))
    test(Power(Variable("x"), Constant(0)).eval, Constant(1))
  }

  @Test def DiffCheck = {
    val a = Variable("x")
    val b = Constant(2)
    val c = Constant(3)
    val d = Constant(4)
    test(Plus(a, b).differentiate, Constant(1))
    assert(Times(a,c).differentiate==Constant(3))
    assert(Plus(Times(a,b), c).differentiate==Constant(2))
    assert(Times(Times(a,b), c).differentiate==Constant(6))
    test(Times(Plus(a,b), Plus(a,c)).eval.toString, "(* (+ x 2) (+ x 3))")
    assert(Times(Plus(a,b), Plus(a,c)).differentiate.toString=="(+ (* x 2) 5)")
  }

  @Test def FunctionsTest = {
    val a = Variable("x")
    val b = Constant(2)
    val c = Constant(3)
    val d = Constant(4)
    test(Ln(Plus(a,b)).eval.toString, "(ln (+ x 2))")
    test(Ln(Plus(b,c)).eval.toString, "1.6094379124341003")
    test(Ln(Plus(a,b)).differentiate.toString, "(/ 1 (+ x 2))")

  }

  @Test def parseCheck = {
//    println(ParseOut.parser("(ln x)"))
//    println(ParseOut.parser("(+ x 3)"))
    println(ParseOut.pars("(* (+ x 2) (+ x 3))"))
  }
  @Test def codeWarsTests = {
    val simpleCases = List(
      "5" -> List("0"),
      "x" -> List("1"),
      "(+ x x)" -> List("2"),
      "(- x x)" -> List("0"),
      "(* x 2)" -> List("2"),
      "(/ x 2)" -> List("0.5"),
      "(^ x 2)" -> List("(* 2 x)"),
      "(cos x)" -> List("(* -1 (sin x))"),
      "(sin x)" -> List("(cos x)"),
      "(tan x)" -> List("(+ 1 (^ (tan x) 2))"),
      "(exp x)" -> List("(exp x)"),
      "(ln x)" -> List("(/ 1 x)")
    )
    val nestedCases = List(
      "(+ x (+ x x))" -> List("3"),
      "(- (+ x x) x)" -> List("1"),
      "(* 2 (+ x 2))" -> List("2"),
      "(/ 2 (+ 1 x))" -> List("(/ -2 (^ (+ 1 x) 2))"),
      "(cos (+ x 1))" -> List("(* -1 (sin (+ x 1)))"),
      "(sin (+ x 1))" -> List("(cos (+ x 1))"),
      "(sin (* 2 x))" -> List("(* 2 (cos (* 2 x)))"),
      "(tan (* 2 x))" -> List("(* 2 (+ 1 (^ (tan (* 2 x)) 2)))"),
      "(exp (* 2 x))" -> List("(* 2 (exp (* 2 x)))"),
      "(cos (* 2 x))" -> List("(* 2 (* -1 (sin (* 2 x))))", "(* -2 (sin (* 2 x)))")
    )
    val secondCases = List(
      "(sin x)" -> List("(* -1 (sin x))"),
      "(exp x)" -> List("(exp x)"),
      "(^ x 3)" -> List("(* 3 (* 2 x))", "(* 6 x)")
    )

    val randomCases = List(
      "(^ (sin x) 3)" -> List ("(* (* 3 (^ (sin x) 2)) (cos x))","(* (cos x) (* 3 (^ (sin x) 2)))","(* (* (^ (sin x) 2) 3) (cos x))","(* (cos x) (* (^ (sin x) 2) 3))")
    )


    for (i <- simpleCases ::: nestedCases) {
      val p = ParseOut.pars(i._1)
      val d = p.differentiate.toString
      println(s"#### pars $p ==? ${i._1}")
      println(s"#### diff $d ==? ${i._2.last}")
      test(d, i._2.last)
    }

    for(i <- secondCases) {
      val p = ParseOut.pars(i._1)
      val d = p.differentiate.differentiate.toString
      println(s"#### pars $p ==? ${i._1}")
      println(s"#### diff $d ==? ${i._2.last}")
      test(d, i._2.last)
    }

    for(i <- randomCases) {
      val p = ParseOut.pars(i._1)
      val d = p.differentiate.toString
      println(s"#### pars $p ==? ${i._1}")
      println(s"#### diff $d ==? ${i._2.head}")
      test(d, i._2.head)
    }

  }



}
