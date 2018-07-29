import training.Chapter2._

object Tests {

  // Test currying
  def sum(a: Int, b: Int): Int = {
    a + b
  }

  val curriedSum: Int => Int => Int = curry(sum)
  val increment = curriedSum(1)

  var x = increment(5)
  x = increment(x)

  println(x)

  // Test un-currying
  def double(a: Int): Int => Int = {
    i: Int => i * a
  }

  val unc: (Int, Int) => Int = uncurry(double)

  println(unc(3, 4))

  def pos(i: Int): Boolean = {
    if (i >= 0) true else false
  }

  def ev(b: Boolean): String = {
    if (b) "yay" else "oh no!!!"
  }

  println("compose: " + compose(ev, pos)(-1))

}