import java.security.AlgorithmParameterGeneratorSpi

object TaglessFinal {

  object ExpressionProblem {
    trait Expr
    case class B(boolean: Boolean) extends  Expr
    case class Or(left: Expr, right: Expr) extends Expr
    case class And(left: Expr, right: Expr) extends Expr
    case class Not(expr: Expr) extends Expr


    val aGiantBoolean: Expr = Or(And(B(true), B(false)), B(false))


    def eval(expr: Expr): Boolean = expr match {
      case B(b) => b
      case Or(a, b) => eval(a) || eval(b)
      case And(a,b) => eval(a) && eval(b)
      case Not(e)=> !eval(e)
    }
  //IF we want to include ints now
    case class I(int: Int) extends Expr
    case class Sum(left: Expr, right: Expr) extends Expr

  //We would need to introduce type casting
    def eval_2(expr: Expr): Any = expr  match {
      case B(b) => b
      case Or(a, b) => eval(a).asInstanceOf[Boolean] || eval(b).asInstanceOf[Boolean]
      case And(a,b) => eval(a) && eval(b)
      case Not(e)=> !eval(e)
    }
  }




  object Solution1 {
    trait Expr{
      val tag: String
    }

    case class B(boolean: Boolean) extends  Expr{
      override val tag: String = "bool"
    }



    case class Or(left: Expr, right: Expr) extends Expr {
      override val tag: String = "bool"
      //You can also put in assert statements if you do not want to check for tag val in eval funtion
      assert(left.tag == "bool" && right.tag == "bool")
    }



    case class And(left: Expr, right: Expr) extends Expr{
      override val tag: String = "bool"
    }
    case class Not(expr: Expr) extends Expr{
      override val tag: String = "bool"
    }

    case class I(int: Int) extends Expr{
      override val tag: String = "int"
    }

    case class Sum(left:Expr, right: Expr) extends Expr{
      override val tag: String = "int"
    }

    //This solution although returns the correct type and still fails if we do not the right tag
    def eval(expr: Expr): Any  = expr match {
      case B(b) => b
      case Or(left, right) => if(left.tag != "bool" || right.tag != "bool") {
                  throw new IllegalArgumentException("Improper Argument Type")
                } else{
                    eval(left).asInstanceOf[Boolean] || eval(right).asInstanceOf[Boolean]
               }
      //Same

      }
    }



  object Tagless{
    trait Expr[A]
    case class B(boolean: Boolean) extends Expr[Boolean]
    case class Or(left: Expr[Boolean], right: Expr[Boolean]) extends Expr[Boolean]
    case class And(left: Expr[Boolean], right: Expr[Boolean]) extends Expr[Boolean]
    case class Not(expr: Expr[Boolean]) extends Expr[Boolean]
    case class I(int: Int) extends Expr[Int]
    case class Sum(left: Expr[Int], right: Expr[Int]) extends Expr[Int]

    def eval[A](expr: Expr[A]): A = expr match {
      case B(b) => b
      case I(i) => i
      case Or(left, right) => eval(left) || eval(right)
      case Sum(left, right) => eval(left) + eval(right)
      // etc
    }
  }


  object TaglessFinalSolution {
    trait Expr[A] {
       val value: A //The Final value we care about
    }

    def b(boolean: Boolean): Expr[Boolean] = new Expr[Boolean]{
      override val value: Boolean = boolean
    }

    def i(int: Int): Expr[Int] = new Expr[Int]{
      override val value: Int = int
    }

    def or(left: Expr[Boolean], right: Expr[Boolean]): Expr[Boolean] = new Expr[Boolean]{
      override val value: Boolean = left.value || right.value
    }

    def sum(left: Expr[Int], right: Expr[Int]): Expr[Int] = new Expr[Int]{
      override val value: Int = left.value + right.value
    }

    //Immediate evaluation
    def eval[A](expr: Expr[A]): A = expr.value


  }

  def demoTagless():Unit = {
    import Tagless._
    println(eval(Or(B(true), And(B(true), B(false)))))
    println(eval(Sum(I(10), I(42))))
    //Correctness proven at compile time, compiler doing type checkign
    //println(eval(Or(B(true), B(42))))
  }


  def demoFinalTagless():Unit = {

    import TaglessFinalSolution._
    println(eval(or(b(true), or(b(true), b(false)))))
    println(eval(sum(i(10), i(42))))
  }



  //Tagless Final is a design Pattern
  //F[_] : Monad = "Tagless Final" -- It solves the expression final. Tagless Final is programming to interfaces for a
  // particular type of problem
  // Type classes is a set of funtionality you offer to some classes

  //Variant of tagless final that uses higher kinded types -- Logic of the expr problem-- they are grouped together under a simple type class
  object TaglessFinalHighKindedTypes {
    trait Algebra[E[_]] { // Also Called Algebra
      def b(boolean: Boolean): E[Boolean]
      def i(int: Int): E[Int]
      def or(left: E[Boolean], right: E[Boolean]): E[Boolean]
      def and(left: E[Boolean], right: E[Boolean]): E[Boolean]
      def sum(left: E[Int], right: E[Int]): E[Int]
    }

    case class SimpleExpr[A](value: A)
    implicit  val simpleAlgebra: Algebra[SimpleExpr]  = new Algebra[SimpleExpr] {
      override def b(boolean: Boolean): SimpleExpr[Boolean] = SimpleExpr(boolean)

      override def i(int: Int): SimpleExpr[Int] = SimpleExpr(int)

      override def or(left: SimpleExpr[Boolean], right: SimpleExpr[Boolean]): SimpleExpr[Boolean] = SimpleExpr(left.value || right.value)

      override def and(left: SimpleExpr[Boolean], right: SimpleExpr[Boolean]): SimpleExpr[Boolean] = SimpleExpr(left.value && right.value)

      override def sum(left: SimpleExpr[Int], right: SimpleExpr[Int]): SimpleExpr[Int] = SimpleExpr(left.value + right.value)
    }

    def program1[E[_]](implicit alg:Algebra[E]): E[Boolean]

  }

  def main(args: Array[String]): Unit = {
    demoTagless()
    demoFinalTagless()
  }
}
