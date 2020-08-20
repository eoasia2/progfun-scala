

class A (value: String)
object A {
  def apply(s: String) = new this(s)
}

case class B (override val value: String)
  extends A(value)

val objB = B("It is a B.value")
val objA = A("It is a A.value")

def methodPrint(f: FunctionPrint[B], obj: B) = {
  f(obj)
}

class FunctionPrint[T <: A](prefix: String) {
  def apply(t: T): Unit = println(prefix + " " + t.value)
}

object FunctionPrint {
  def apply[T <: A](prefix: String) = new FunctionPrint[T](prefix)
}

//val printA = FunctionPrint[A]("A-value:")
//val printB = FunctionPrint[B]("B-value:")

class FunctionPrint[T <: A](prefix: String) {
  def apply(t: T): Unit = println(prefix + " " + t.value)
}

//methodPrint(printB, objB)
//methodPrint(printA, objB)