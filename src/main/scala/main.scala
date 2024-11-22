import scala.collection.mutable

type Environment = mutable.Map[String, mutable.Map[String, Double]] // Map from gate name to variables
type GlobalEnvironment = mutable.Map[String, Double] // Global variable environment

object FuzzyLogicDSL:

  given env: Environment = mutable.Map() // Environment for storing variables within specific gates
  val globalEnv: GlobalEnvironment = mutable.Map() // Global environment for variables not tied to a specific gate

  // Abstract class for fuzzy expressions, with the 'eval' method for evaluation
  abstract class FuzzyExpr:
    def eval(localEnv: mutable.Map[String, Double] = mutable.Map(), gateName: String = ""): Double
    def partialEval(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]] = mutable.Map(), gateName: String = ""): FuzzyExpr = this

  // Classes for the class system
  case class ClassDef(
                       name: String,
                       variables: mutable.Map[String, Double] = mutable.Map(),
                       methods: mutable.Map[String, MethodDef] = mutable.Map(),
                       parent: Option[ClassDef] = None,
                       nestedClasses: mutable.Map[String, ClassDef] = mutable.Map()
                     )

  case class MethodDef(name: String, parameters: List[Parameter], body: List[Statement])

  case class Parameter(name: String, paramType: String)

  abstract class Statement:
    def execute(localEnv: mutable.Map[String, Double]): Unit
    def partialExecute(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]]): Unit = {}

  case class Assignment(variable: String, expr: FuzzyExpr) extends Statement:
    def execute(localEnv: mutable.Map[String, Double]): Unit =
      localEnv(variable) = expr.eval(localEnv)

    override def partialExecute(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]]): Unit =
      val peExpr = expr.partialEval(localEnv)
      peExpr match
        case FuzzyValue(v) => localEnv(variable) = Left(v)
        case _ => localEnv(variable) = Right(peExpr)

  case class ReturnStatement(expr: FuzzyExpr) extends Statement:
    def execute(localEnv: mutable.Map[String, Double]): Unit =
      throw ReturnException(expr.eval(localEnv))

    override def partialExecute(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]]): Unit =
      val peExpr = expr.partialEval(localEnv)
      peExpr match
        case FuzzyValue(v) => throw ReturnException(v)
        case _ => throw PartialReturnException(peExpr)

  case class ExpressionStatement(expr: FuzzyExpr) extends Statement:
    def execute(localEnv: mutable.Map[String, Double]): Unit =
      expr.eval(localEnv) // Evaluate the expression, but ignore result

    override def partialExecute(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]]): Unit =
      expr.partialEval(localEnv) // Partially evaluate the expression, but ignore result

  case class ConditionalStatement(cond: Condition, thenBranch: List[Statement], elseBranch: List[Statement]) extends Statement:
    def execute(localEnv: mutable.Map[String, Double]): Unit =
      if cond.eval(localEnv) then
        thenBranch.foreach(_.execute(localEnv))
      else
        elseBranch.foreach(_.execute(localEnv))

    override def partialExecute(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]]): Unit =
      val peCond = cond.partialEval(localEnv)
      peCond match
        case ComparisonValue(true) =>
          thenBranch.foreach(_.partialExecute(localEnv))
        case ComparisonValue(false) =>
          elseBranch.foreach(_.partialExecute(localEnv))
        case _ =>
          // Can't decide which branch, so we partially evaluate both branches
          thenBranch.foreach(_.partialExecute(localEnv))
          elseBranch.foreach(_.partialExecute(localEnv))

  private case class ReturnException(value: Double) extends Exception
  private case class PartialReturnException(expr: FuzzyExpr) extends Exception

  case class Instance(classDef: ClassDef, variables: mutable.Map[String, Double] = mutable.Map())

  // The class registry holds all class definitions
  val classRegistry: mutable.Map[String, ClassDef] = mutable.Map()

  // Stack to keep track of the current class context
  private val currentClassStack: mutable.Stack[ClassDef] = mutable.Stack()

  // Function to define a class
  def Class(name: String, parent: Option[ClassDef] = None)(body: => Unit = ()): ClassDef =
    val classDef = ClassDef(name, parent = parent)
    if currentClassStack.nonEmpty then
      val currentClass = currentClassStack.top
      currentClass.nestedClasses(name) = classDef
    else
      classRegistry(name) = classDef
    currentClassStack.push(classDef)
    try
      body
    finally
      currentClassStack.pop()
    classDef

  // Function to specify inheritance
  def Extends(parentClass: ClassDef): Option[ClassDef] = Some(parentClass)

  // Function to define a class variable
  def ClassVar(name: String, varType: String): Unit =
    val currentClass = currentClassStack.top
    if varType != "double" then
      throw new Exception(s"Only 'double' type is supported, but got '$varType'")
    currentClass.variables(name) = 0.0 // Initialize to 0.0

  // Function to define a method
  def DefineMethod(name: String, parameters: List[Parameter], body: List[Statement]): Unit =
    val currentClass = currentClassStack.top
    currentClass.methods(name) = MethodDef(name, parameters, body)

  // Function to create a new instance of a class
  def CreateNew(classDef: ClassDef): Instance =
    val instanceVariables = mutable.Map[String, Double]()
    // Initialize variables, including inherited variables
    def collectVariables(c: ClassDef): Unit =
      if c.parent.isDefined then collectVariables(c.parent.get)
      c.variables.foreach { case (name, value) =>
        if !instanceVariables.contains(name) then
          instanceVariables(name) = value
      }
    collectVariables(classDef)
    Instance(classDef, instanceVariables)

  // Function to invoke a method on an instance
  def InvokeMethod(instance: Instance, methodName: String, args: Map[String, Double]): Double =
    // Find method in class hierarchy
    def findMethod(c: ClassDef): Option[MethodDef] =
      c.methods.get(methodName) match
        case someMethod@Some(_) => someMethod
        case None => c.parent.flatMap(findMethod)
    findMethod(instance.classDef) match
      case Some(methodDef) =>
        // Create a local environment for method execution
        val localVariables = mutable.Map[String, Double]()
        // Add instance variables
        localVariables ++= instance.variables
        // Add parameters
        methodDef.parameters.foreach { param =>
          if args.contains(param.name) then
            localVariables(param.name) = args(param.name)
          else
            throw new Exception(s"Missing argument for parameter ${param.name}")
        }
        // Execute the method body
        try
          methodDef.body.foreach(_.execute(localVariables))
          // If no return statement, return 0.0
          0.0
        catch
          case ReturnException(value) => value
        finally
          // Update instance variables with all variables from localVariables
          instance.variables ++= localVariables
      case None => throw new Exception(s"Method $methodName not found in class hierarchy of ${instance.classDef.name}")

  // Function to partially invoke a method on an instance
  def PartialInvokeMethod(instance: Instance, methodName: String, args: Map[String, Either[Double, FuzzyExpr]]): Either[Double, FuzzyExpr] =
    // Find method in class hierarchy
    def findMethod(c: ClassDef): Option[MethodDef] =
      c.methods.get(methodName) match
        case someMethod@Some(_) => someMethod
        case None => c.parent.flatMap(findMethod)
    findMethod(instance.classDef) match
      case Some(methodDef) =>
        // Create a local environment for method execution
        val localVariables = mutable.Map[String, Either[Double, FuzzyExpr]]()
        // Add instance variables
        instance.variables.foreach { case (k, v) => localVariables(k) = Left(v) }
        // Add parameters
        methodDef.parameters.foreach { param =>
          if args.contains(param.name) then
            localVariables(param.name) = args(param.name)
          else
            throw new Exception(s"Missing argument for parameter ${param.name}")
        }
        // Execute the method body
        try
          methodDef.body.foreach(_.partialExecute(localVariables))
          // If no return statement, return 0.0
          Left(0.0)
        catch
          case ReturnException(value) => Left(value)
          case PartialReturnException(expr) => Right(expr)
        finally
          // Update instance variables with all variables from localVariables that have concrete values
          localVariables.foreach {
            case (k, Left(v)) => instance.variables(k) = v
            case _ => // Do nothing
          }
      case None => throw new Exception(s"Method $methodName not found in class hierarchy of ${instance.classDef.name}")

  // Functions to create statements
  def Assign(variable: String, expr: FuzzyExpr): Assignment = Assignment(variable, expr)
  def Return(expr: FuzzyExpr): ReturnStatement = ReturnStatement(expr)
  def Expr(expr: FuzzyExpr): ExpressionStatement = ExpressionStatement(expr)
  def IfTrue(cond: Condition)(thenBranch: List[Statement])(elseBranch: List[Statement]): ConditionalStatement =
    ConditionalStatement(cond, thenBranch, elseBranch)

  // Fuzzy Expressions
  case class FuzzyValue(v: Double) extends FuzzyExpr:
    def eval(localEnv: mutable.Map[String, Double] = mutable.Map(), gateName: String = ""): Double = v
    override def partialEval(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]] = mutable.Map(), gateName: String = ""): FuzzyExpr = this

  case class FuzzyVariable(name: String) extends FuzzyExpr:
    def eval(localEnv: mutable.Map[String, Double] = mutable.Map(), gateName: String = ""): Double =
      localEnv.getOrElse(name,
        // Check the gate's environment if gateName is provided
        if gateName != "" && summon[Environment].contains(gateName) then
          summon[Environment](gateName).getOrElse(name,
            globalEnv.getOrElse(name, throw new Exception(s"Variable $name not found")))
        else
          globalEnv.getOrElse(name, throw new Exception(s"Variable $name not found"))
      )

    override def partialEval(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]] = mutable.Map(), gateName: String = ""): FuzzyExpr =
      localEnv.get(name) match
        case Some(Left(value)) => FuzzyValue(value)
        case Some(Right(expr)) => expr
        case None =>
          // Check the gate's environment if gateName is provided
          if gateName != "" && summon[Environment].contains(gateName) then
            summon[Environment](gateName).get(name) match
              case Some(value) => FuzzyValue(value)
              case None => this // Undefined, keep as variable
          else
            globalEnv.get(name) match
              case Some(value) => FuzzyValue(value)
              case None => this // Undefined, keep as variable

  // Fuzzy Operations
  private case class FuzzyAdd(e1: FuzzyExpr, e2: FuzzyExpr) extends FuzzyExpr:
    def eval(localEnv: mutable.Map[String, Double] = mutable.Map(), gateName: String = ""): Double =
      math.min(1.0, e1.eval(localEnv, gateName) + e2.eval(localEnv, gateName))

    override def partialEval(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]] = mutable.Map(), gateName: String = ""): FuzzyExpr =
      val pe1 = e1.partialEval(localEnv, gateName)
      val pe2 = e2.partialEval(localEnv, gateName)
      (pe1, pe2) match
        case (FuzzyValue(v1), FuzzyValue(v2)) =>
          FuzzyValue(math.min(1.0, v1 + v2))
        case _ =>
          FuzzyAdd(pe1, pe2)

  private case class FuzzyMult(e1: FuzzyExpr, e2: FuzzyExpr) extends FuzzyExpr:
    def eval(localEnv: mutable.Map[String, Double] = mutable.Map(), gateName: String = ""): Double =
      e1.eval(localEnv, gateName) * e2.eval(localEnv, gateName)

    override def partialEval(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]] = mutable.Map(), gateName: String = ""): FuzzyExpr =
      val pe1 = e1.partialEval(localEnv, gateName)
      val pe2 = e2.partialEval(localEnv, gateName)
      (pe1, pe2) match
        case (FuzzyValue(v1), FuzzyValue(v2)) =>
          FuzzyValue(v1 * v2)
        case _ =>
          FuzzyMult(pe1, pe2)

  private case class FuzzyComplement(e: FuzzyExpr) extends FuzzyExpr:
    def eval(localEnv: mutable.Map[String, Double] = mutable.Map(), gateName: String = ""): Double =
      1.0 - e.eval(localEnv, gateName)

    override def partialEval(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]] = mutable.Map(), gateName: String = ""): FuzzyExpr =
      val pe = e.partialEval(localEnv, gateName)
      pe match
        case FuzzyValue(v) => FuzzyValue(1.0 - v)
        case _ => FuzzyComplement(pe)

  private case class FuzzyAND(e1: FuzzyExpr, e2: FuzzyExpr) extends FuzzyExpr:
    def eval(localEnv: mutable.Map[String, Double] = mutable.Map(), gateName: String = ""): Double =
      math.min(e1.eval(localEnv, gateName), e2.eval(localEnv, gateName)) // AND is min

    override def partialEval(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]] = mutable.Map(), gateName: String = ""): FuzzyExpr =
      val pe1 = e1.partialEval(localEnv, gateName)
      val pe2 = e2.partialEval(localEnv, gateName)
      (pe1, pe2) match
        case (FuzzyValue(v1), FuzzyValue(v2)) =>
          FuzzyValue(math.min(v1, v2))
        case _ =>
          FuzzyAND(pe1, pe2)

  private case class FuzzyOR(e1: FuzzyExpr, e2: FuzzyExpr) extends FuzzyExpr:
    def eval(localEnv: mutable.Map[String, Double] = mutable.Map(), gateName: String = ""): Double =
      math.max(e1.eval(localEnv, gateName), e2.eval(localEnv, gateName)) // OR is max

    override def partialEval(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]] = mutable.Map(), gateName: String = ""): FuzzyExpr =
      val pe1 = e1.partialEval(localEnv, gateName)
      val pe2 = e2.partialEval(localEnv, gateName)
      (pe1, pe2) match
        case (FuzzyValue(v1), FuzzyValue(v2)) =>
          FuzzyValue(math.max(v1, v2))
        case _ =>
          FuzzyOR(pe1, pe2)

  private case class FuzzyXOR(e1: FuzzyExpr, e2: FuzzyExpr) extends FuzzyExpr:
    def eval(localEnv: mutable.Map[String, Double] = mutable.Map(), gateName: String = ""): Double =
      math.abs(e1.eval(localEnv, gateName) - e2.eval(localEnv, gateName)) // XOR formula

    override def partialEval(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]] = mutable.Map(), gateName: String = ""): FuzzyExpr =
      val pe1 = e1.partialEval(localEnv, gateName)
      val pe2 = e2.partialEval(localEnv, gateName)
      (pe1, pe2) match
        case (FuzzyValue(v1), FuzzyValue(v2)) =>
          FuzzyValue(math.abs(v1 - v2))
        case _ =>
          FuzzyXOR(pe1, pe2)

  private case class FuzzyAlphaCut(e: FuzzyExpr, alpha: Double) extends FuzzyExpr:
    def eval(localEnv: mutable.Map[String, Double] = mutable.Map(), gateName: String = ""): Double =
      if e.eval(localEnv, gateName) >= alpha then e.eval(localEnv, gateName) else 0.0 // Alpha Cut

    override def partialEval(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]] = mutable.Map(), gateName: String = ""): FuzzyExpr =
      val pe = e.partialEval(localEnv, gateName)
      pe match
        case FuzzyValue(v) =>
          if v >= alpha then FuzzyValue(v) else FuzzyValue(0.0)
        case _ =>
          FuzzyAlphaCut(pe, alpha)

  // Conditional Expression
  case class ConditionalExpr(cond: Condition, thenExpr: FuzzyExpr, elseExpr: FuzzyExpr) extends FuzzyExpr:
    def eval(localEnv: mutable.Map[String, Double] = mutable.Map(), gateName: String = ""): Double =
      if cond.eval(localEnv, gateName) then
        thenExpr.eval(localEnv, gateName)
      else
        elseExpr.eval(localEnv, gateName)

    override def partialEval(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]] = mutable.Map(), gateName: String = ""): FuzzyExpr =
      val peCond = cond.partialEval(localEnv, gateName)
      peCond match
        case ComparisonValue(true) =>
          thenExpr.partialEval(localEnv, gateName)
        case ComparisonValue(false) =>
          elseExpr.partialEval(localEnv, gateName)
        case _ =>
          val peThen = thenExpr.partialEval(localEnv, gateName)
          val peElse = elseExpr.partialEval(localEnv, gateName)
          ConditionalExpr(peCond, peThen, peElse)

  // Condition classes
  abstract class Condition:
    def eval(localEnv: mutable.Map[String, Double] = mutable.Map(), gateName: String = ""): Boolean
    def partialEval(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]] = mutable.Map(), gateName: String = ""): Condition = this

  case class GreaterEqual(left: FuzzyExpr, right: FuzzyExpr) extends Condition:
    def eval(localEnv: mutable.Map[String, Double] = mutable.Map(), gateName: String = ""): Boolean =
      left.eval(localEnv, gateName) >= right.eval(localEnv, gateName)

    override def partialEval(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]] = mutable.Map(), gateName: String = ""): Condition =
      val peLeft = left.partialEval(localEnv, gateName)
      val peRight = right.partialEval(localEnv, gateName)
      (peLeft, peRight) match
        case (FuzzyValue(vLeft), FuzzyValue(vRight)) =>
          ComparisonValue(vLeft >= vRight)
        case _ => GreaterEqual(peLeft, peRight)

  case class LessThan(left: FuzzyExpr, right: FuzzyExpr) extends Condition:
    def eval(localEnv: mutable.Map[String, Double] = mutable.Map(), gateName: String = ""): Boolean =
      left.eval(localEnv, gateName) < right.eval(localEnv, gateName)

    override def partialEval(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]] = mutable.Map(), gateName: String = ""): Condition =
      val peLeft = left.partialEval(localEnv, gateName)
      val peRight = right.partialEval(localEnv, gateName)
      (peLeft, peRight) match
        case (FuzzyValue(vLeft), FuzzyValue(vRight)) =>
          ComparisonValue(vLeft < vRight)
        case _ => LessThan(peLeft, peRight)

  case class ComparisonValue(value: Boolean) extends Condition:
    def eval(localEnv: mutable.Map[String, Double] = mutable.Map(), gateName: String = ""): Boolean = value
    override def partialEval(localEnv: mutable.Map[String, Either[Double, FuzzyExpr]] = mutable.Map(), gateName: String = ""): Condition = this

  // Assign variable to local/global scope or gate to expression
  def Assign(left: Any, right: FuzzyExpr)(using gate: Gate = Gate("global")): Unit = left match
    case FuzzyVariable(name) =>
      if gate.name == "global" then // If global assign global scope
        globalEnv.update(name, right.eval())
      else
        summon[Environment].getOrElseUpdate(gate.name, mutable.Map[String, Double]()).update(name, right.eval())
    // Assign a fuzzy expression to a Gate (store it in the gate system)
    case gate: Gate => gateSystem.gates(gate.name) = right
    case _ => throw new Exception("Invalid assignment")

  // Define a scope for a specific gate, allowing assignments and operations within the gate
  def Scope(gate: Gate)(block: Gate ?=> Unit): Unit =
    given Gate = gate
    block

  // Define an anonymous scope that temporarily modifies variables and restores them after execution
  def AnonymousScope(block: => Unit): Unit =
    // Save current environment specific gates
    val originalEnv = summon[Environment].map { case (k, v) => k -> v.clone() }
    val originalGlobalEnv = globalEnv.clone()
    try
      block
    finally
      // Restore environments and globals
      summon[Environment].clear()
      summon[Environment].addAll(originalEnv)
      globalEnv.clear()
      globalEnv.addAll(originalGlobalEnv)

  // Test the logic gate result using the active bindings in the gate's context or globally
  def TestGate(gateName: String, variableName: String): Double =
    // Check if a gate exists and the variable is scoped within that gate
    if summon[Environment].contains(gateName) then
      summon[Environment](gateName).get(variableName) match
        case Some(value) => value
        // Look in global scope if not in local
        case None => globalEnv.getOrElse(variableName, throw new Exception(s"Variable $variableName not found globally or in gate $gateName"))
    else
      // If no gate look at global
      globalEnv.getOrElse(variableName, throw new Exception(s"Variable $variableName not found globally or in gate $gateName"))

  // Evaluate the expression assigned to the gate, if any
  def EvaluateGateExpression(gateName: String): Double =
    gateSystem.gates.get(gateName) match
      case Some(expr) =>
        val localEnv = summon[Environment].getOrElse(gateName, mutable.Map())
        expr.eval(localEnv, gateName)
      case None => throw new Exception(s"No expression assigned to gate $gateName")

  // Functions for fuzzy operations
  def ADD(e1: FuzzyExpr, e2: FuzzyExpr): FuzzyExpr = FuzzyAdd(e1, e2)
  def MULT(e1: FuzzyExpr, e2: FuzzyExpr): FuzzyExpr = FuzzyMult(e1, e2)
  def COMPLEMENT(e: FuzzyExpr): FuzzyExpr = FuzzyComplement(e)
  def AND(e1: FuzzyExpr, e2: FuzzyExpr): FuzzyExpr = FuzzyAND(e1, e2)
  def OR(e1: FuzzyExpr, e2: FuzzyExpr): FuzzyExpr = FuzzyOR(e1, e2)
  def XOR(e1: FuzzyExpr, e2: FuzzyExpr): FuzzyExpr = FuzzyXOR(e1, e2)
  def ALPHA_CUT(e: FuzzyExpr, alpha: Double): FuzzyExpr = FuzzyAlphaCut(e, alpha)

  // Functions for conditions
  def IFTRUE(cond: Condition)(thenExpr: FuzzyExpr)(elseExpr: FuzzyExpr): FuzzyExpr = ConditionalExpr(cond, thenExpr, elseExpr)
  def IFTRUE_STMT(cond: Condition)(thenBranch: List[Statement])(elseBranch: List[Statement]): ConditionalStatement = ConditionalStatement(cond, thenBranch, elseBranch)
  def GREATEREQUAL(e1: FuzzyExpr, e2: FuzzyExpr): Condition = GreaterEqual(e1, e2)
  def LESSTHAN(e1: FuzzyExpr, e2: FuzzyExpr): Condition = LessThan(e1, e2)

  // Gate and GateSystem definitions remain unchanged
  case class Gate(name: String) // Define a gate by name
  case class GateSystem(gates: mutable.Map[String, FuzzyExpr] = mutable.Map()) // Holds all gates and their associated fuzzy expressions
  val gateSystem: GateSystem = GateSystem()

object Main:
  import FuzzyLogicDSL.*

  def main(args: Array[String]): Unit =
    println("Running Partial Evaluation and Conditional Tests")

    // Test partial evaluation of an expression
    val expr = MULT(FuzzyValue(3), MULT(ADD(FuzzyValue(5), FuzzyValue(1)), FuzzyVariable("var")))
    val partialExpr = expr.partialEval()
    println(s"Partial Evaluation of expression: $partialExpr")
    // Expected: MULT(FuzzyValue(3.0), MULT(FuzzyValue(6.0), FuzzyVariable(var)))

    // Assign "var" later
    globalEnv("var") = 2.0
    val evaluatedExpr = expr.eval()
    println(s"Full Evaluation after assigning var: $evaluatedExpr")
    // Expected: 3 * 1 * 2 = 6

    // Test partial evaluation with associativity
    val expr2 = MULT(FuzzyVariable("newVar"), MULT(FuzzyValue(5), FuzzyValue(3)))
    val partialExpr2 = expr2.partialEval()
    println(s"Partial Evaluation with associativity: $partialExpr2")
    // Expected: MULT(FuzzyVariable(var), FuzzyValue(15.0))

    // Conditional expression example
    val condExpr = IFTRUE(GREATEREQUAL(MULT(FuzzyValue(15), FuzzyVariable("VAR")), ADD(FuzzyValue(2), FuzzyVariable("var1"))))(
      ADD(FuzzyVariable("VAR"), FuzzyValue(3))
    )(
      FuzzyValue(0.0)
    )
    val partialCondExpr = condExpr.partialEval()
    println(s"Partial Evaluation of Conditional Expression: $partialCondExpr")
    // Expected: ConditionalExpr with partially evaluated condition and branches

    // Assign values to "var" and "var1"
    globalEnv("VAR") = 0.2
    globalEnv("var1") = 0.1

    val evaluatedCondExpr = condExpr.eval()
    println(s"Evaluation of Conditional Expression after assigning variables: $evaluatedCondExpr")
    // Evaluate the condition and the appropriate branch

    // Test partial evaluation in method invocation
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
    println(s"Partial Invocation Result: $partialResult")
    // Expected: Right(partially evaluated expression)

    // Assign values and fully invoke the method
    baseInstance.variables("var") = 0.3
    val fullResult = InvokeMethod(baseInstance, "m1", Map("p1" -> 1.0, "p2" -> 0.5))
    println(s"Full Invocation Result after assigning variables: $fullResult")
// Expected: 0.6
