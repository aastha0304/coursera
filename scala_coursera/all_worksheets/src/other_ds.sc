object mergesort {
//    def msort(a: List[Int]): List[Int] ={
//        if(a.length < 2) a
//        else{
//            def merge(xs: List[Int], ys: List[Int]) : List[Int] = (xs, ys ) match {
//                case (xs, Nil) => xs
//                case (Nil, ys) => ys
//                case (x::xend, y::yend) => if (x < y) x :: merge(xend, ys) else y :: merge(xs, yend)
//            }
//            val (fst, snd) = a splitAt a.length/2
//            merge(msort(fst), msort(snd))
//        }
//
//    }
    
    def msort[T](a: List[T])(f: (T,T) => Boolean): List[T] ={
        if(a.length < 2) a
        else{
            def merge(xs: List[T], ys: List[T]) : List[T] = (xs, ys ) match {
                case (xs, Nil) => xs
                case (Nil, ys) => ys
                case (x::xend, y::yend) => if (f(x,y)) x :: merge(xend, ys) else y :: merge(xs, yend)
            }
            val (fst, snd) = a splitAt a.length/2
            merge(msort(fst)(f), msort(snd)(f))
        }
        
    }
    
    msort(List[Int](8,9,7,7,5,2,3,0))((x,y) => x<y)
}