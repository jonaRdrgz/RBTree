
object Color extends Enumeration {
  type Color = Value
  val Red, Black = Value
}

sealed trait Tree[A] { def color: Color.Color }
case class Empty[A]() extends Tree[A] { def color = Color.Black }
case class Full[A](color: Color.Color, a: A, l: Tree[A], r: Tree[A]) extends Tree[A]

object Tree {
  import Color._
  import Ordering.Implicits._

  def fold[A,X](e: X, f: (Color, A, X, X) => X, t: Tree[A]): X =
    t match {
      case Empty() => e
      case B(l,x,r) => f(Black, x, fold(e, f, l), fold(e, f, r))
      case R(l,x,r) => f(Black, x, fold(e, f, l), fold(e, f, r))
    }


  def empty[A: Ordering]: Tree[A] = Empty[A]

  def member[A: Ordering](x: A, t: Tree[A]): Boolean =
    t match {
      case Empty() => false
      case Full(_, y, l, r) =>
        if (x < y)
          member(x, l)
        else if (x > y)
          member(x, r)
        else
          true

    }

  def elements[A: Ordering](t: Tree[A]): List[A] = {

    def aux(t: Tree[A], acc: List[A]): List[A] =
      t match {
        case Empty() => acc
        case Full(_, a, l, r) => aux(l, a :: aux(r, acc))
      }

    aux(t, List.empty[A])
  }

  def insert[A: Ordering](a: A, t: Tree[A]): Tree[A] =
    blacken(ins(a, t))

  private def ins[A: Ordering](x: A, t: Tree[A]): Tree[A] =
    t match {
      case Empty() => Full(Red, x, Empty(), Empty())
      case Full(c, y, l, r) =>
        if (x < y)
          balance(Full(c, y, ins(x, l), r))
        else if (x > y)
          balance(Full(c, y, l, ins(x, r)))
        else
          t
    }

  object B {
    def apply[A](l: Tree[A], a: A, r: Tree[A]): Tree[A] =
      Full(Black, a, l, r)

    def unapply[A](t: Tree[A]): Option[(Tree[A], A, Tree[A])] =
      t match {
        case Full(Black, a, l, r) => Some((l, a, r))
        case _ => None
      }
  }

  object R {
    def apply[A](l: Tree[A], a: A, r: Tree[A]): Tree[A] =
      Full(Red, a, l, r)

    def unapply[A](t: Tree[A]): Option[(Tree[A], A, Tree[A])] =
      t match {
        case Full(Red, a, l, r) => Some((l, a, r))
        case _ => None
      }
  }

  private def balance[A: Ordering](t: Tree[A]): Tree[A] =
    t match {
      case B(R(R(a,x,b),y,c),z,d) => R(B(a,x,b),y,B(c,z,d))
      case B(R(a,x,R(b,y,c)),z,d) => R(B(a,x,b),y,B(c,z,d))
      case B(a,x,R(R(b,y,c),z,d)) => R(B(a,x,b),y,B(c,z,d))
      case B(a,x,R(b,y,R(c,z,d))) => R(B(a,x,b),y,B(c,z,d))
      case _ => t
    }

  private def blacken[A](t: Tree[A]): Tree[A] =
    t match {
      case Empty() => t
      case tt@Full(_,_,_,_) => tt.copy(color = Black)
    }

}

val tree: Tree[Int] =
  (1 to 9).foldLeft(Tree.empty[Int]) { case (acc, n) =>
    Tree.insert(n, acc)
  }

val s =
  Tree.fold[Int, (Int, String)]( (0,"."), { case (c,a,l,r)  =>
  (l._1 + 1, s"$a-${c}\n${" " * l._1}${l._2}-${r._2}")}, tree)

println(s._2)
