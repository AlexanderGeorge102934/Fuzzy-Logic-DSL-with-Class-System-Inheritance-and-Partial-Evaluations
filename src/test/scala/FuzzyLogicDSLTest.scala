import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import FuzzyLogicDSL._

class FuzzyLogicDSLTest extends AnyFlatSpec with Matchers {

  behavior of "FuzzyLogicDSL with Partial Evaluation and Conditionals"

  it should "partially evaluate expressions correctly" in {
    // Clear environments before the test
    FuzzyLogicDSL.env.clear()
    FuzzyLogicDSL.globalEnv.clear()
    FuzzyLogicDSL.gateSystem.gates.clear()

    // Test partial evaluation of an expression
    val expr = MULT(FuzzyValue(3), MULT(ADD(FuzzyValue(5), FuzzyValue(1)), FuzzyVariable("var")))
    val partialExpr = expr.partialEval()
    partialExpr shouldBe MULT(FuzzyValue(3.0), MULT(FuzzyValue(1.0), FuzzyVariable("var")))

    // Assign "var" later
    globalEnv("var") = 2.0
    val evaluatedExpr = expr.eval()
    evaluatedExpr shouldBe 3 * 1 * 2  // Expected: 36.0
  }

  it should "partially evaluate expressions with associativity correctly" in {
    val expr2 = MULT(FuzzyVariable("newVar"), MULT(FuzzyValue(5), FuzzyValue(3)))
    val partialExpr2 = expr2.partialEval()
    partialExpr2 shouldBe MULT(FuzzyVariable("newVar"), FuzzyValue(15.0)) // 5 * 3 = 15
  }

  it should "partially evaluate conditional expressions with GREATEREQUAL" in {
    // Clear environments before the test
    FuzzyLogicDSL.env.clear()
    FuzzyLogicDSL.globalEnv.clear()
    FuzzyLogicDSL.gateSystem.gates.clear()

    val condExpr = IFTRUE(GREATEREQUAL(MULT(FuzzyValue(15), FuzzyVariable("VAR")), ADD(FuzzyValue(2), FuzzyVariable("var1"))))(
      ADD(FuzzyVariable("VAR"), FuzzyValue(3))
    )(
      FuzzyValue(0.0)
    )
    val partialCondExpr = condExpr.partialEval()
    partialCondExpr shouldBe a [ConditionalExpr]

    // Assign values to "VAR" and "var1"
    globalEnv("VAR") = 0.2
    globalEnv("var1") = 0.1

    val evaluatedCondExpr = condExpr.eval()
    evaluatedCondExpr shouldBe 1.0  // Due to ADD cap at 1.0
  }

  it should "evaluate conditional expressions choosing the correct branch" in {
    // Clear environments before the test
    FuzzyLogicDSL.env.clear()
    FuzzyLogicDSL.globalEnv.clear()
    FuzzyLogicDSL.gateSystem.gates.clear()

    val condExpr = IFTRUE(GREATEREQUAL(FuzzyVariable("x"), FuzzyValue(0.5)))(
      FuzzyValue(1.0)
    )(
      FuzzyValue(0.0)
    )

    // Assign x = 0.7, condition should be true
    globalEnv("x") = 0.7
    condExpr.eval() shouldBe 1.0
  }

  it should "evaluate conditional expressions with LESSTHAN correctly" in {
    // Clear environments before the test
    FuzzyLogicDSL.env.clear()
    FuzzyLogicDSL.globalEnv.clear()
    FuzzyLogicDSL.gateSystem.gates.clear()

    val condExpr = IFTRUE(LESSTHAN(FuzzyVariable("x"), FuzzyValue(0.5)))(
      FuzzyValue(1.0)
    )(
      FuzzyValue(0.0)
    )

    // Partially evaluate the expression
    val partialCondExpr = condExpr.partialEval()
    partialCondExpr shouldBe a [ConditionalExpr]

    // Assign x = 0.7, condition should be false
    globalEnv("x") = 0.7
    condExpr.eval() shouldBe 0.0
  }

  it should "partially evaluate conditional expressions with LESSTHAN" in {
    // Clear environments before the test
    FuzzyLogicDSL.env.clear()
    FuzzyLogicDSL.globalEnv.clear()
    FuzzyLogicDSL.gateSystem.gates.clear()

    val condExpr = IFTRUE(LESSTHAN(FuzzyVariable("x"), FuzzyVariable("y")))(
      ADD(FuzzyVariable("x"), FuzzyValue(0.2))
    )(
      FuzzyValue(0.0)
    )

    // Partial evaluation should keep the condition and branches
    val partialCondExpr = condExpr.partialEval()
    partialCondExpr shouldBe ConditionalExpr(
      LessThan(FuzzyVariable("x"), FuzzyVariable("y")),
      ADD(FuzzyVariable("x"), FuzzyValue(0.2)),
      FuzzyValue(0.0)
    )

    // Now assign x = 0.3, y = 0.5
    globalEnv("x") = 0.3
    globalEnv("y") = 0.5

    // Condition x < y is true, should evaluate thenExpr
    condExpr.eval() shouldBe math.min(1.0, 0.3 + 0.2) // Expected: 0.5
  }

  it should "execute conditional statements with IFTRUE_STMT and LESSTHAN" in {
    // Clear environments before the test
    FuzzyLogicDSL.env.clear()
    FuzzyLogicDSL.globalEnv.clear()
    FuzzyLogicDSL.gateSystem.gates.clear()

    // Define a method with IFTRUE_STMT
    val MyClass = Class("MyClass") {
      ClassVar("value", "double")

      DefineMethod("checkAndAssign",
        List(Parameter("input", "double")),
        List(
          IfTrue(LESSTHAN(FuzzyVariable("input"), FuzzyValue(0.5)))(
            List(
              Assign("value", FuzzyValue(0.1))
            )
          )(
            List(
              Assign("value", FuzzyValue(0.9))
            )
          ),
          Return(FuzzyVariable("value"))
        )
      )
    }

    val myInstance = CreateNew(MyClass)

    // Invoke method with input = 0.3
    val result1 = InvokeMethod(myInstance, "checkAndAssign", Map("input" -> 0.3))
    result1 shouldBe 0.1
  }

  it should "partially invoke methods and return partially evaluated expressions" in {
    // Clear environments before the test
    FuzzyLogicDSL.env.clear()
    FuzzyLogicDSL.globalEnv.clear()
    FuzzyLogicDSL.gateSystem.gates.clear()

    val BaseClass = Class("Base") {
      ClassVar("var", "double")

      DefineMethod("m1",
        List(Parameter("p1", "double"), Parameter("p2", "double")),
        List(
          Assign("somevar", MULT(FuzzyVariable("var"), FuzzyVariable("p1"))),
          Return(MULT(FuzzyVariable("somevar"), FuzzyValue(2.0)))
        )
      )
    }

    val baseInstance = CreateNew(BaseClass)

    // Partially invoke method m1
    val partialResult = PartialInvokeMethod(baseInstance, "m1", Map("p1" -> Right(FuzzyVariable("x")), "p2" -> Left(0.5)))
    partialResult match {
      case Right(expr) =>
        expr shouldBe a [FuzzyExpr]
      case Left(value) =>
        fail(s"Expected a partially evaluated expression, but got a concrete value: $value")
    }

    // Assign values and fully invoke the method
    baseInstance.variables("var") = 0.3
    globalEnv("x") = 1.0  // Since p1 was assigned FuzzyVariable("x")
    val fullResult = InvokeMethod(baseInstance, "m1", Map("p1" -> 1.0, "p2" -> 0.5))
    fullResult shouldBe 0.6
  }

  it should "perform partial evaluation within method bodies correctly" in {
    // Define a method with a conditional
    val MyClass = Class("MyClass") {
      ClassVar("threshold", "double")

      DefineMethod("process",
        List(Parameter("input", "double")),
        List(
          IfTrue(GREATEREQUAL(FuzzyVariable("input"), FuzzyVariable("threshold")))(
            List(
              Assign("result", MULT(FuzzyVariable("input"), FuzzyValue(2.0))),
              Return(FuzzyVariable("result"))
            )
          )(
            List(
              Assign("result", FuzzyValue(0.0)),
              Return(FuzzyVariable("result"))
            )
          )
        )
      )
    }

    val myInstance = CreateNew(MyClass)
    myInstance.variables("threshold") = 0.5

    // Partially invoke the method with undefined "input"
    val partialResult = PartialInvokeMethod(myInstance, "process", Map("input" -> Right(FuzzyVariable("x"))))

    val fullResult = InvokeMethod(myInstance, "process", Map("input" -> 0.3))
    fullResult shouldBe 0.0
  }

  it should "correctly handle partial evaluation of expressions with undefined variables" in {
    // Expression with undefined variables
    val expr = ADD(FuzzyVariable("a"), MULT(FuzzyValue(2.0), FuzzyVariable("b")))
    val partialExpr = expr.partialEval()
    partialExpr shouldBe ADD(FuzzyVariable("a"), MULT(FuzzyValue(2.0), FuzzyVariable("b")))

    // Now define "b" and partially evaluate again
    globalEnv("b") = 0.5
    val partialExpr2 = expr.partialEval()
    partialExpr2 shouldBe ADD(FuzzyVariable("a"), FuzzyValue(1.0)) // Since 2.0 * 0.5 = 1.0

    // Now define "a" and fully evaluate
    globalEnv("a") = 0.3
    val evaluatedExpr = expr.eval()
    evaluatedExpr shouldBe math.min(1.0, 0.3 + 1.0) // ADD caps at 1.0, so result is 1.0
  }

  it should "correctly perform alpha cuts in partial evaluations" in {
    // Clear environments before the test
    FuzzyLogicDSL.env.clear()
    FuzzyLogicDSL.globalEnv.clear()
    FuzzyLogicDSL.gateSystem.gates.clear()

    val expr = ALPHA_CUT(FuzzyVariable("x"), 0.5)
    val partialExpr = expr.partialEval()
    partialExpr shouldBe ALPHA_CUT(FuzzyVariable("x"), 0.5)

    // Assign x = 0.7, should result in 0.7 after eval
    globalEnv("x") = 0.7
    expr.eval() shouldBe 0.7
  }

}
