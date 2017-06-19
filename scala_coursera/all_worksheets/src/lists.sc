def reverse(a: List[Int]) : List[Int] = a match {
    case Nil => Nil
    case x::xs => reverse(a.tail) ::: List(a.head)
}

def reverseList(a: List[Int]) : List[Int] = a match {
    case Nil => Nil
    case x::xs => reverseListHelper(List(x), xs)
}

def reverseListHelper(res: List[Int], orig: List[Int]) : List[Int] = orig match {
    case Nil => res
    case x::xs => reverseListHelper(x :: res, xs)
}
def insertSorted(x: Int, xs: List[Int]) : List[Int] = xs match {
    case Nil => List(x)
    case y::ys => if (x <= xs.head) x :: xs else xs.head :: insertSorted(x, xs.tail)
}

def isort(a: List[Int]) : List[Int] = a match {
    case Nil => Nil
    case x::xs => insertSorted(a.head, isort(a.tail))
}

def last[T](a: List[T]): T = a match {
    case Nil => throw new NoSuchElementException
    case x::xs => if(xs == Nil) x else last(xs)
}

def init[T](a: List[T]): List[T] = a match {
    case Nil => throw new NoSuchElementException
    case x::xs => if(xs == Nil) List() else x :: init(xs)
}

def times(chars: List[Char]): List[(Char, Int)] = chars.map( x=> (x,1)).groupBy(_._1).mapValues(_.size).to[List]

def concat[T](a: List[T], b: List[T]) : List[T] = a match {
    case Nil => b
    case x::xs => x :: concat(xs , b) // concat (xs, x::b) gives reverse of a
}

def removeAt[T](n: Int, a: List[T]) : List[T] = (a take n) ++ (a drop n+1)
times(List('a','b','c','a','c'))

isort(List(98,5,555))

reverse(List(1,2,3,4,5))

reverseList(List(1,2,3, 4, 5))

last(List[Int](1,2,3,4,5))

init(List[Int](1,2,3,4,5))

concat(List[Int](1,2,3,4,5), List[Int](1,2,3,4,5))

removeAt( 4, concat(List[Int](1,2,3,4,5), List[Int](1,2,3,4,5)) )