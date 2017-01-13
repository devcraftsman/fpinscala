package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

    def size[A](t: Tree[A]) : Int = t match {
        case Leaf(_) => 1
        case Branch(l,r) => 1 + size(l)+size(r)
      }

    def maximum(t: Tree[Int]) : Int =
     t match {
        case Leaf(x) => x
        case Branch(l,r)=> maximum(l) max maximum(r)

        }

    def depth[A](t:Tree[A]): Int =  t  match {
      case Leaf(_) => 0
      case Branch(l,r) => 1 + (depth(l) max depth(r))
    }

    def map[A,B](t: Tree[A])(f: A=>B) : Tree[B] = t match {
      case Leaf(e) => Leaf(f(e))
      case Branch(l,r) => Branch(map(l)(f),map(r)(f))
    }



    def fold[A,B](t: Tree[A])(f: A=>B)(g: (B,B)=>B) : B = t match {
      case Leaf(n) => f(n)
      case Branch(l,r) => g(fold(l)(f)(g),fold(r)(f)(g))
    }

    def size2(t: Tree[Int]) : Int = fold(t)((_) =>1)(_ + _)

    def maximum2(t: Tree[Int]) : Int = fold(t)((x) =>x)(_ max _)

    def depth2(t: Tree[Int]) : Int = fold(t)((x) =>0)((d1,d2) =>1 +(d1 max d2))

    def map2[A,B](t: Tree[A])(m: A=>B) : Tree[B] = fold[A,Tree[B]](t)((x) => Leaf(m(x)))(Branch(_,_))












}
