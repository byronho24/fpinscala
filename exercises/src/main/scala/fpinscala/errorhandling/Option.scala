package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  // Apply f if Option is not None.
  def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(v) => Some(f(v))
  }

  // Apply f, which may fail, to the Option if not None
  def flatMap[B](f: A => Option[B]): Option[B] = 
      this.map(f).getOrElse(None)

  // Get value inside Option if not None, else get something else
  // B has to at least include A, since return value can be of type A (when Option is not None)
  // default: => B means that only evaluate default if we need it - lazy evaluation
  def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(v) => v
  }

  // Returns this Option if this Option is not None, else returns the Option in the arg.
  // ? When would this be useful?
  def orElse[B >: A](ob: => Option[B]): Option[B] = 
      this.map(Some(_)).getOrElse(ob)
  // Turns Some into None if filter condition not met. If None already, return None
  def filter(f: A => Boolean): Option[A] = {
      val keep = this.map(f).getOrElse(false)
      if (keep) this else None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  // When would the different methods on Option be useful?
  // map: Proceed with the computation if an error has not occurred
  //      This also acts to delay the error handling process
  //      So (Something that might fail).map(Do the next thing)
  //      is like saying "Do this; if this doesn't fail, do the next thing"
  // flatMap: same as above, except the `next thing` might fail as well
  // getOrElse: use this when you actually want to handle the error
  // orElse: chain together two computations that can fail
  //         i.e. (A).orElse(B) is like saying 
  //              `if A fails, *try* B instead` (try because we don't know if B would succeed!)
  // getOrElse can be used to turn None back to a plain old Exception - 
  // should only do that if no reasonable program would be able to
  // recover from the exception
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = ???

  def sequence[A](a: List[Option[A]]): Option[List[A]] = ???

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = ???
}