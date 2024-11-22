# Fuzzy Logic DSL with Class System and Inheritance

## Overview

This project implements a **Domain-Specific Language (DSL)** for handling **fuzzy logic operations**, enhanced with an object-oriented **class system** that supports inheritance, method overriding, nested classes, and partial evaluation. The DSL allows you to create and manage fuzzy variables, apply common fuzzy logic operations, and scope variables both globally and within specific "gates" (logical contexts). Additionally, it provides functionality for partial evaluation of expressions and methods, enabling optimization and symbolic computation when not all variables are defined.

## Table of Contents

- [Key Concepts](#key-concepts)
- [DSL Functions and Syntax](#dsl-functions-and-syntax)
  - [Fuzzy Variables and Values](#fuzzy-variables-and-values)
  - [Fuzzy Operations](#fuzzy-operations)
  - [Conditions](#conditions)
  - [Conditional Expressions and Statements](#conditional-expressions-and-statements)
  - [Class Definitions](#class-definitions)
  - [Defining Class Variables](#defining-class-variables)
  - [Class Methods](#class-methods)
  - [Nested Classes](#nested-classes)
  - [Creating Instances](#creating-instances)
  - [Method Invocation](#method-invocation)
  - [Scopes and Gates](#scopes-and-gates)
  - [Assigning Variables and Expressions](#assigning-variables-and-expressions)
  - [Evaluating Gate Expressions](#evaluating-gate-expressions)
  - [Anonymous Scopes](#anonymous-scopes)
  - [Partial Evaluation](#partial-evaluation)
- [Error Handling](#error-handling)
- [Project Structure](#project-structure)
- [Getting Started](#getting-started)
  - [Prerequisites](#prerequisites)
  - [Setting Up the Project](#setting-up-the-project)
  - [Running the Project](#running-the-project)
  - [Running Tests](#running-tests)
- [Examples](#examples)
  - [Partial Evaluation and Conditional Tests](#partial-evaluation-and-conditional-tests)
  - [Class Inheritance and Method Invocation](#class-inheritance-and-method-invocation)
  - [Using Gates and Scopes](#using-gates-and-scopes)
- [Limitations and Rules](#limitations-and-rules)
- [Conclusion](#conclusion)

## Key Concepts

### Fuzzy Variables and Values

- **Fuzzy Variable (`FuzzyVariable`)**: Represents a variable that can take on values between 0.0 and 1.0. These variables can be assigned specific values using the `Assign` function. Assignments can be global or scoped to a particular gate.

- **Fuzzy Value (`FuzzyValue`)**: A concrete numerical value between 0.0 and 1.0 that represents a degree of truth in fuzzy logic.

### Fuzzy Expressions (`FuzzyExpr`)

Fuzzy expressions represent operations or transformations applied to fuzzy variables and values. They are the core building blocks of computations in the DSL.

### Class System and Inheritance

The DSL includes an object-oriented class system that supports:

- **Classes**: Defined with variables and methods.

- **Inheritance**: Classes can inherit variables and methods from a parent class (single inheritance).

- **Method Overriding**: A subclass can provide a specific implementation of a method already defined in its superclass.

- **Nested Classes**: Classes defined within another class, allowing hierarchical structuring and encapsulation.

### Scopes and Gates

Variables can exist either in a **global scope** or within **local scopes**, referred to as **gates**. Gates represent logical contexts where variables can be defined, and fuzzy logic expressions can be assigned and evaluated.

### Anonymous Scopes

Temporary environments that allow you to execute code within a block, ensuring that changes within the block do not persist once the block exits.

### Partial Evaluation

Partial evaluation allows expressions to be simplified based on the current knowledge of variable values. If not all variables are defined, the expression remains partially evaluated, keeping undefined variables symbolic. This is useful for optimization and symbolic computation.

## DSL Functions and Syntax

### Fuzzy Variables and Values

- **Defining a Fuzzy Variable**:
  ```scala
  FuzzyVariable("variableName")
  ```

- **Defining a Fuzzy Value**:
  ```scala
  FuzzyValue(0.5) // Value between 0.0 and 1.0
  ```

### Fuzzy Operations

The DSL supports various fuzzy operations, which can be combined to form complex expressions.

- **Addition (`ADD`)**:
  ```scala
  ADD(expr1, expr2) // Capped at 1.0
  ```
  Example:
  ```scala
  ADD(FuzzyVariable("A"), FuzzyVariable("B"))
  ```

- **Multiplication (`MULT`)**:
  ```scala
  MULT(expr1, expr2)
  ```
  Example:
  ```scala
  MULT(FuzzyValue(0.5), FuzzyVariable("C"))
  ```

- **Complement (`COMPLEMENT`)**:
  ```scala
  COMPLEMENT(expr)
  ```
  Example:
  ```scala
  COMPLEMENT(FuzzyVariable("D"))
  ```

- **AND (`AND`)**:
  ```scala
  AND(expr1, expr2) // Takes the minimum of two expressions
  ```
  Example:
  ```scala
  AND(FuzzyVariable("A"), FuzzyVariable("B"))
  ```

- **OR (`OR`)**:
  ```scala
  OR(expr1, expr2) // Takes the maximum of two expressions
  ```
  Example:
  ```scala
  OR(FuzzyVariable("A"), FuzzyVariable("B"))
  ```

- **XOR (`XOR`)**:
  ```scala
  XOR(expr1, expr2) // Absolute difference between two expressions
  ```
  Example:
  ```scala
  XOR(FuzzyVariable("A"), FuzzyVariable("B"))
  ```

- **Alpha Cut (`ALPHA_CUT`)**:
  ```scala
  ALPHA_CUT(expr, alpha) // Threshold operation
  ```
  Example:
  ```scala
  ALPHA_CUT(FuzzyVariable("E"), 0.5)
  ```

### Conditions

Conditions are used in conditional expressions and statements.

- **Greater or Equal (`GREATEREQUAL`)**:
  ```scala
  GREATEREQUAL(expr1, expr2)
  ```
  Example:
  ```scala
  GREATEREQUAL(FuzzyVariable("A"), FuzzyValue(0.7))
  ```

- **Less Than (`LESSTHAN`)**:
  ```scala
  LESSTHAN(expr1, expr2)
  ```
  Example:
  ```scala
  LESSTHAN(FuzzyVariable("B"), FuzzyVariable("C"))
  ```

### Conditional Expressions and Statements

- **Conditional Expression (`IFTRUE`)**:
  ```scala
  IFTRUE(condition)(
    thenExpr
  )(
    elseExpr
  )
  ```
  Example:
  ```scala
  val expr = IFTRUE(LESSTHAN(FuzzyVariable("x"), FuzzyValue(0.5)))(
    FuzzyValue(1.0)
  )(
    FuzzyValue(0.0)
  )
  ```

- **Conditional Statement (`IFTRUE_STMT`)**:
  ```scala
  IFTRUE_STMT(condition)(
    thenBranch: List[Statement]
  )(
    elseBranch: List[Statement]
  )
  ```
  Example:
  ```scala
  IFTRUE_STMT(GREATEREQUAL(FuzzyVariable("score"), FuzzyValue(0.8)))(
    List(
      Assign("grade", FuzzyValue(1.0))
    )
  )(
    List(
      Assign("grade", FuzzyValue(0.0))
    )
  )
  ```

### Class Definitions

- **Defining a Class**:
  ```scala
  val ClassName = Class("ClassName") {
    // Class variables and methods
  }
  ```
  Example:
  ```scala
  val BaseClass = Class("Base") {
    ClassVar("var", "double")
    // Define methods
  }
  ```

- **Defining a Class with Inheritance**:
  ```scala
  val SubClass = Class("SubClass", Extends(ParentClass)) {
    // Additional variables and methods
  }
  ```
  Example:
  ```scala
  val DerivedClass = Class("Derived", Extends(BaseClass)) {
    // Additional variables and methods
  }
  ```

### Defining Class Variables

- **Defining a Class Variable**:
  ```scala
  ClassVar("variableName", "double")
  ```
  Note: All class variables must be of type `"double"`.

### Class Methods

- **Defining a Class Method**:
  ```scala
  DefineMethod("methodName", List(Parameter("param1", "type"), ...), List(
    // Statements
    Return(expression)
  ))
  ```
  Example:
  ```scala
  DefineMethod("m1", List(Parameter("p1", "double")), List(
    Assign("somevar", ADD(FuzzyVariable("var"), FuzzyVariable("p1"))),
    Return(FuzzyVariable("somevar"))
  ))
  ```

### Nested Classes

- **Defining a Nested Class**:
  ```scala
  Class("OuterClass") {
    // Outer class definitions
    Class("InnerClass") {
      // Inner class definitions
    }
  }
  ```
  Example:
  ```scala
  val OuterClass = Class("Outer") {
    ClassVar("outerVar", "double")
    Class("Inner") {
      ClassVar("innerVar", "double")
      DefineMethod("innerMethod", List(), List(
        Assign("innerVar", ADD(FuzzyVariable("innerVar"), FuzzyVariable("outerVar"))),
        Return(FuzzyVariable("innerVar"))
      ))
    }
  }
  ```

### Creating Instances

- **Creating an Instance of a Class**:
  ```scala
  val instance = CreateNew(ClassName)
  ```
  Example:
  ```scala
  val baseInstance = CreateNew(BaseClass)
  ```

- **Creating an Instance of a Nested Class**:
  ```scala
  val innerClassDef = OuterClass.nestedClasses("Inner")
  val innerInstance = CreateNew(innerClassDef)
  ```

### Method Invocation

- **Invoking a Method**:
  ```scala
  val result = InvokeMethod(instance, "methodName", Map("param1" -> value1, ...))
  ```
  Example:
  ```scala
  val result = InvokeMethod(baseInstance, "m1", Map("p1" -> 0.5))
  ```

- **Partially Invoking a Method**:
  ```scala
  val partialResult = PartialInvokeMethod(instance, "methodName", Map("param1" -> Right(FuzzyVariable("x")), ...))
  ```
  Example:
  ```scala
  val partialResult = PartialInvokeMethod(baseInstance, "m1", Map("p1" -> Right(FuzzyVariable("x"))))
  ```

**Note**: When partially invoking methods, avoid using `IFTRUE_STMT` if the method has no parameters, as it may lead to unexpected behavior.

### Scopes and Gates

- **Defining a Gate**:
  ```scala
  Gate("gateName")
  ```

- **Entering a Gate Scope**:
  ```scala
  Scope(Gate("gateName")) {
    // Code within the gate
  }
  ```
  Example:
  ```scala
  Scope(Gate("logicGate1")) {
    Assign(FuzzyVariable("A"), FuzzyValue(0.5))
    Assign(FuzzyVariable("B"), FuzzyValue(0.7))
  }
  ```

### Assigning Variables and Expressions

- **Assigning a Variable Globally**:
  ```scala
  Assign(FuzzyVariable("X"), FuzzyValue(0.2))
  ```

- **Assigning a Variable within a Gate**:
  ```scala
  Scope(Gate("myGate")) {
    Assign(FuzzyVariable("A"), FuzzyValue(0.5))
  }
  ```

- **Assigning an Expression to a Gate**:
  ```scala
  Assign(Gate("logicGate1"), ADD(FuzzyVariable("A"), FuzzyVariable("B")))(using Gate("logicGate1"))
  ```

### Evaluating Gate Expressions

- **Evaluating an Expression Assigned to a Gate**:
  ```scala
  val result = EvaluateGateExpression("logicGate1")
  ```

- **Testing a Variable's Value in a Scope**:
  ```scala
  val value = TestGate("gateName", "variableName")
  ```
  Example:
  ```scala
  val xValue = TestGate("global", "X")
  ```

### Anonymous Scopes

- **Using an Anonymous Scope**:
  ```scala
  AnonymousScope {
    // Temporary assignments and operations
  }
  ```
  Example:
  ```scala
  AnonymousScope {
    Assign(FuzzyVariable("Y"), FuzzyValue(0.9))
    // Changes to Y are temporary within this block
  }
  // Outside the block, Y reverts to its previous value
  ```

### Partial Evaluation

- **Partially Evaluating an Expression**:
  ```scala
  val partialExpr = expr.partialEval()
  ```

- **Partially Evaluating a Conditional Expression**:
  ```scala
  val partialCondExpr = condExpr.partialEval()
  ```

- **Partially Invoking a Method**:
  ```scala
  val partialResult = PartialInvokeMethod(instance, "methodName", Map("param1" -> Right(FuzzyVariable("x")), ...))
  ```

- **Invoking a Method with Full Evaluation**:
  ```scala
  val result = InvokeMethod(instance, "methodName", Map("param1" -> 0.5, ...))
  ```

## Error Handling

- **Undefined Variables**: Attempting to evaluate an expression with undefined variables will throw an exception.
  ```scala
  throw new Exception(s"Variable $name not found globally or in gate $gateName")
  ```

- **Unassigned Gate Expressions**: Evaluating a gate without an assigned expression will throw an exception.
  ```scala
  throw new Exception(s"No expression assigned to gate $gateName")
  ```

- **Missing Parameters in Method Invocation**: Invoking a method without providing all required parameters will throw an exception.
  ```scala
  throw new Exception(s"Missing argument for parameter ${param.name}")
  ```

- **Invalid Assignments**: Assigning invalid types or to invalid targets will throw an exception.
  ```scala
  throw new Exception("Invalid assignment")
  ```

## Project Structure

- `src/main/scala/FuzzyLogicDSL.scala` – Main code for the DSL implementation.
- `src/main/scala/Main.scala` – Main program demonstrating usage of the DSL.
- `src/test/scala/FuzzyLogicDSLTest.scala` – Test suite for the DSL.
- `build.sbt` – SBT configuration file for building and managing dependencies.

## Getting Started

### Prerequisites

- **Scala**: Version 2.13.x or higher.
- **SBT**: Simple Build Tool (version 1.10.1 or higher).
- **Java Development Kit (JDK)**: Version 8 to 22.
- **IntelliJ IDEA**: Recommended IDE for Scala development.

### Setting Up the Project

1. **Clone the Repository**:
   ```bash
   git clone <repository-url>
   cd <repository-directory>
   ```

2. **Open the Project in IntelliJ IDEA**:
   - Open IntelliJ IDEA.
   - Select **Open** and navigate to the cloned repository folder.
   - IntelliJ will detect the `build.sbt` file and set up the project dependencies.
   - If prompted, click **Enable Auto-Import** to automatically download and manage the Scala dependencies via SBT.

3. **Verify JDK Installation**:
   - Ensure that a compatible JDK is installed and configured in IntelliJ IDEA.

4. **Compile the Project**:
   - In the IntelliJ terminal or your system terminal, navigate to the project root and run:
     ```bash
     sbt clean compile
     ```

### Running the Project

- **Compile the Project**:
  ```bash
  sbt compile
  ```

- **Run the Main Program**:
  ```bash
  sbt run
  ```

### Running Tests

- **Run the Test Suite**:
  ```bash
  sbt test
  ```

## Examples

### Partial Evaluation and Conditional Tests

```scala
import FuzzyLogicDSL._

// Partial evaluation of an expression
val expr = MULT(FuzzyValue(3), MULT(ADD(FuzzyValue(5), FuzzyValue(1)), FuzzyVariable("var")))
val partialExpr = expr.partialEval()
println(s"Partial Evaluation of expression: $partialExpr")
// Output: MULT(FuzzyValue(3.0), MULT(FuzzyValue(6.0), FuzzyVariable(var)))

// Assign "var" later
globalEnv("var") = 2.0
val evaluatedExpr = expr.eval()
println(s"Full Evaluation after assigning var: $evaluatedExpr")
// Output: 36.0 (3 * 6 * 2)

// Conditional expression example
val condExpr = IFTRUE(GREATEREQUAL(MULT(FuzzyValue(15), FuzzyVariable("VAR")), ADD(FuzzyValue(2), FuzzyVariable("var1"))))(
  ADD(FuzzyVariable("VAR"), FuzzyValue(3))
)(
  FuzzyValue(0.0)
)
val partialCondExpr = condExpr.partialEval()
println(s"Partial Evaluation of Conditional Expression: $partialCondExpr")

// Assign values to "VAR" and "var1"
globalEnv("VAR") = 0.2
globalEnv("var1") = 0.1

val evaluatedCondExpr = condExpr.eval()
println(s"Evaluation of Conditional Expression after assigning variables: $evaluatedCondExpr")
// Output: 1.0 (due to ADD cap at 1.0)
```

### Class Inheritance and Method Invocation

```scala
import FuzzyLogicDSL._

// Define Base Class
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

// Create an instance of BaseClass
val baseInstance = CreateNew(BaseClass)

// Partially invoke method m1
val partialResult = PartialInvokeMethod(baseInstance, "m1", Map("p1" -> Right(FuzzyVariable("x")), "p2" -> Left(0.5)))
println(s"Partial Invocation Result: $partialResult")
// Output: Right(partially evaluated expression)

// Assign values and fully invoke the method
baseInstance.variables("var") = 0.3
val fullResult = InvokeMethod(baseInstance, "m1", Map("p1" -> 1.0, "p2" -> 0.5))
println(s"Full Invocation Result after assigning variables: $fullResult")
// Output: 0.6
```

### Using Gates and Scopes

```scala
import FuzzyLogicDSL._

// Assign global variable
Assign(FuzzyVariable("X"), FuzzyValue(0.2)) // Global assignment

// Assign variables in a gate scope
Scope(Gate("logicGate1")) {
  Assign(FuzzyVariable("A"), FuzzyValue(0.5))
  Assign(FuzzyVariable("B"), FuzzyValue(0.7))
}

// Assign an expression to a gate and evaluate it
Assign(Gate("logicGate1"), ADD(FuzzyVariable("A"), FuzzyVariable("B")))(using Gate("logicGate1"))
val gateResult = EvaluateGateExpression("logicGate1")
println(s"Evaluation of logicGate1: $gateResult")
// Output: 1.0 (due to ADD cap at 1.0)

// Test variable values
println(s"Value of X in global scope: ${TestGate("global", "X")}")
// Output: 0.2
```

## Limitations and Rules

- **Partial Evaluation and `IFTRUE_STMT`**:
  - **Rule**: Do not use `IFTRUE_STMT` when partially invoking a class method that has no arguments, as it can lead to unexpected behavior.
  - **Explanation**: Partial evaluation relies on the ability to handle symbolic variables. If a method with no arguments contains an `IFTRUE_STMT`, and variables used in the condition are undefined, it may not be possible to partially evaluate the method correctly.

- **Variable Scoping**:
  - Variables defined within a gate are not accessible outside that gate.
  - Global variables are accessible within any gate unless overridden.

- **Variable Assignments in Classes**:
  - Class variables cannot be assigned values directly within the class definition. They must be assigned within methods or after instance creation.

- **Method Parameters**:
  - All parameters must be provided when invoking a method. Missing parameters will result in an exception.

- **Data Types**:
  - All variables and parameters must be of type `"double"` (representing fuzzy values between 0.0 and 1.0).
