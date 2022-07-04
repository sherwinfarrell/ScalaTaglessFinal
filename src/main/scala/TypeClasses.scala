

object TypeClassesSimple {

  //Type Classes are type system constructs that support AdHoc Polymorphism
  // Specialized Implementations
  //Generic Method

  def processMyList[T](list: List[T]): T = {
    // Sum up all the elements in a list
    //if the list is integers then it is actual sum
    //If string then it is the cocatenation
    // We need different implementations for different types

    //Constraint: For other types we get an error -- We shouln't use this for other types apart from integer or strings
    //For C++ -> Template Specilizations For Scala --> implicits

    ???
  }

  trait Summable[T] {
    def sumElements(list: List[T]): T
  }

  implicit object IntSummable extends Summable[Int] {
    override def sumElements(list: List[Int]): Int = list.sum
  }

  implicit object StringSummable extends Summable[String]{
    override def sumElements(list: List[String]): String = list.mkString(" ")
  }


  def processMyListImplicit[T](list: List[T])(implicit summable: Summable[T]): T = { //Ad-Hoc Polymorphism
    summable.sumElements(list)
  }

  def main(args: Array[String]): Unit = {
    val intSum = processMyListImplicit(List(1,2,3)) //IntSummable injected implicitly
    val stringSum = processMyListImplicit(List("Hello", "World", "!"))
    println(intSum)
    println(stringSum)

    //If I want to use processMyList of booleans then the compiler with throw no implicits found
    //val booleanSum = processMyListImplicit(List(true, false, true)) --> Throws error at compile time
  }
}
