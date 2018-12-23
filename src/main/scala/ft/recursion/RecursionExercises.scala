package ft.recursion

// Taken from http://tmorris.net/posts/scala-exercises-for-beginners/index.html

/**
 * Ok here are the rules.
 *
 * You can't use any of the standard list functions, like `map`, `filter`, `flatMap`, `append`, `:::`, `:+`, etc.
 * 
 * But you can always use `::` to construct a new list by prepending an element to another list.
 *
 * You CAN and are encouraged to use the solutions from the exercises below to solve the harder
 * ones towards the end.
 *
 * Keep an eye out for repetition and similarities between your answers.
 *
 * REMEMBER: Follow the types, they almost always guide you to the solution.  If it compiles and looks a little
 * too simple, it's probably correct.  As Sherlock Holmes once said, "Each one is suggestive, together they are
 * most certainly conclusive."
 *
 * See if you can make your solution tail recursive, where possible.
 *
 */

object RecursionExercises  extends App {

  def plusOne(n: Int) = n + 1

  def minusOne(n: Int) = n - 1

  // Add two non-negative Integers together.  You are only allowed to use plusOne and minusOne above
  def add(a: Int, b: Int): Int = if (b == 0) a else add(plusOne(a), minusOne(b))

  //You are not permitted to use any list functions such as map, flatMap, ++, flatten etc
  def sum(l: List[Int]): Int = l match {
    case x :: xs => add(x, sum(xs))
    case Nil => 0
  }


  //Again no list functions are permitted for the following
  def length[A](x: List[A]): Int = x match {
    case x :: xs => add(1, length(xs))
    case Nil => 0
  }

  // Do you notice anything similar between sum and length? Hmm...

  // Mapping over a list.  You are given a List of type A and a function converting an A to a B
  // and you give back a list of type B.  No list functions allowed!
  def map[A, B](x: List[A], f: A => B): List[B] = x match {
    case x :: xs => f(x) :: map(xs, f)
    case Nil => List[B]()
  }

  // Given a function from A => Boolean, return a list with only those item where the function returned true.
  def filter[A](x: List[A], f: A => Boolean): List[A] = x match {
    case x :: xs => if (f(x)) x :: filter(xs, f) else filter(xs, f)
    case Nil => List[A]()
    }

  // This pattern should be familiar by now... psst... look at add.
  def append[A](x: List[A], y: List[A]): List[A] = x match {
    case x :: xs => x :: append(xs, y)
    case Nil => y match{
      case y :: ys => y :: append(x, ys)
      case Nil => List[A]()
    }
  }

  // Flatten a list of lists to a single list.  Remember you can't use list.flatten.  Can you use a previous
  // solution to solve this one?
  def flatten[A](x: List[List[A]]): List[A] = x match{
    case x :: xs => append(x, flatten(xs))
    case Nil => List[A]()
  }

  // Follow the types.  You've done a great job getting here. Follow the types.
  def flatMap[A, B](x: List[A], f: A => List[B]): List[B] = x match {
    case x :: xs => append(f(x), flatMap(xs, f))
    case Nil => List[B]()
  }

  // Maximum of the empty list is 0
  def maximum(x: List[Int]): Int = x match {
    case x :: xs => if (x > maximum(xs)) x else maximum(xs)
    case Nil => 0
    case _ => 0
  }

  // Reverse a list
  def reverse[A](x: List[A]): List[A] = x match {
    case x :: xs => append(reverse(xs), List[A](x))
    case Nil => List[A]()
  }

}
