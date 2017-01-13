package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x,xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = Cons(h,l)

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_,t) => drop(t, n-1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Cons(x,xs) if (f(x)) => dropWhile(xs,f)
      case _ => l
    }


  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x,Nil) => Nil
    case Cons(x,xs) => Cons(x,init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l,0)((_,x) => x+1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Nil => z
      case Cons(x,xs) => foldLeft(xs,f(z, x))(f)
    }

    def sum3(ns: List[Int]) =
      foldLeft(ns, 0)((x,y) => x + y)

    def product3(ns: List[Double]) =
      foldLeft(ns, 1.0)(_ * _)

    def length2[A](l: List[A]): Int = foldLeft(l,0)((x,_) => x+1)

    def append2[A](a1: List[A], a2: List[A]): List[A] =
      foldRight(a1,a2)((l,a) => Cons(l,a))

    def concat[A](l:List[List[A]]) : List[A] =
      foldRight(l,Nil: List[A])(append)

    def add1(l :List[Int]) : List[Int] = l match {
      case Nil => Nil
      case Cons(x,t) => Cons(x+1,add1(t))
    }

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x,t) => Cons(f(x),map(t)(f))
  }

  def filter[A](l:List[A])(f: A => Boolean): List[A] = {
    foldRight(l, Nil:List[A])((h,t) => if (f(h)) Cons(h,t) else t)
  }

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    //foldRight(l, Nil:List[B])((h,t) => append(f(h),t))
    concat(map(l)(f))

 def addList(as1 :List[Int],as2 :List[Int]) : List[Int] = (as1,as2) match {
   case (Nil, _) => Nil
   case (_, Nil) => Nil
   case (Cons(h1,t1), Cons(h2,t2)) => Cons(h1+h2, addList(t1,t2))
 }

 def zipWith[A,B,C](as1 :List[A],as2 :List[B])(f: (A,B) => C) : List[C] = (as1,as2) match {
   case (Nil, _) => Nil
   case (_, Nil) => Nil
   case (Cons(h1,t1), Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f))
 }


}
