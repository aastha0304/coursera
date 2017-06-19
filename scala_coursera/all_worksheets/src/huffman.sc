
object Huffman {
    
    abstract class CodeTree {
        def contains(head: Char): Boolean
    }
    
    case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree {
        override def contains(head: Char): Boolean = if (chars contains head ) true else  left.contains(head) || right.contains(head)
    }
    
    case class Leaf(char: Char, weight: Int) extends CodeTree {
        override def contains(head: Char): Boolean = if (char==head) true else false
    }
    
    def weight(tree: CodeTree): Int = tree match {
        case Fork(_, _, _, wgt) => wgt
        case Leaf(_, wgt) => wgt
    } // tree match ...
    
    def chars(tree: CodeTree): List[Char] = tree match{
        case Fork(_, _, chars, _) => chars
        case Leaf(char, _) => List(char)
    } // tree match ...
    
    def makeCodeTree(left: CodeTree, right: CodeTree) =
        Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

    def string2Chars(str: String): List[Char] = str.toList

    def times(chars: List[Char]): List[(Char, Int)] = chars.map( x=> (x,1)).groupBy(_._1).mapValues(_.size).to[List]
    
    def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = freqs.map( x =>  Leaf(x._1, x._2)).sortBy( x => x.weight )
    
    def singleton(trees: List[CodeTree]): Boolean = trees.size == 1
    
    def combine(trees: List[CodeTree]): List[CodeTree] = if(trees.size<3) trees else combine( makeCodeTree( trees.head, trees.tail.head ) :: trees.drop(2))
    
    def until(xxx: List[CodeTree] => Boolean, yyy: List[CodeTree] => List[CodeTree])(zzz: List[CodeTree]): List[CodeTree] =
        if(xxx(zzz)) zzz else until(xxx, yyy)( yyy (zzz))
    
    def createCodeTree(chars: List[Char]): CodeTree = until(singleton, combine ) ( makeOrderedLeafList ( times( chars ) ) ).head
    
    type Bit = Int
    
    def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
        def inner(tree: CodeTree)(bit: Bit): List[Char] = tree match {
                case Fork(left, right, _, _) => if (bit == 0) inner(left)(bit) else inner(right)(bit)
                case Leaf(char, _) => List(char)
        }
        bits flatMap inner(tree)
    }

    val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s',121895),Fork(Leaf('d',56269),Fork(Fork(Fork(Leaf('x',5928),Leaf('j',8351),List('x','j'),14279),Leaf('f',16351),List('x','j','f'),30630),Fork(Fork(Fork(Fork(Leaf('z',2093),Fork(Leaf('k',745),Leaf('w',1747),List('k','w'),2492),List('z','k','w'),4585),Leaf('y',4725),List('z','k','w','y'),9310),Leaf('h',11298),List('z','k','w','y','h'),20608),Leaf('q',20889),List('z','k','w','y','h','q'),41497),List('x','j','f','z','k','w','y','h','q'),72127),List('d','x','j','f','z','k','w','y','h','q'),128396),List('s','d','x','j','f','z','k','w','y','h','q'),250291),Fork(Fork(Leaf('o',82762),Leaf('l',83668),List('o','l'),166430),Fork(Fork(Leaf('m',45521),Leaf('p',46335),List('m','p'),91856),Leaf('u',96785),List('m','p','u'),188641),List('o','l','m','p','u'),355071),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u'),605362),Fork(Fork(Fork(Leaf('r',100500),Fork(Leaf('c',50003),Fork(Leaf('v',24975),Fork(Leaf('g',13288),Leaf('b',13822),List('g','b'),27110),List('v','g','b'),52085),List('c','v','g','b'),102088),List('r','c','v','g','b'),202588),Fork(Leaf('n',108812),Leaf('t',111103),List('n','t'),219915),List('r','c','v','g','b','n','t'),422503),Fork(Leaf('e',225947),Fork(Leaf('i',115465),Leaf('a',117110),List('i','a'),232575),List('e','i','a'),458522),List('r','c','v','g','b','n','t','e','i','a'),881025),List('s','d','x','j','f','z','k','w','y','h','q','o','l','m','p','u','r','c','v','g','b','n','t','e','i','a'),1486387)

    val secret: List[Bit] = List(0,0,1,1,1,0,1,0,1,1,1,0,0,1,1,0,1,0,0,1,1,0,1,0,1,1,0,0,1,1,1,1,1,0,1,0,1,1,0,0,0,0,1,0,1,1,1,0,0,1,0,0,1,0,0,0,1,0,0,0,1,0,1)
    
    def decodedSecret: List[Char] = decode(frenchCode, secret)
    
//    def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
//        def inner(tree: CodeTree) (char: Char): List[Bit] = tree match {
//            case Fork(left, right, _, _) => if( left.contains(char) ) 0 :: inner(left)(char) else 1 :: inner(right)(char)
//            case Leaf(char, _) => List(char)
//        }
//        text flatMap inner(tree)
//    }
    
    def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
        def lookup(tree:  CodeTree)(c: Char): List[Bit] = tree match {
            case Leaf(_, _) => List()
            case Fork(left, right, _, _) => if (left contains c) 0 :: lookup(left) (c) else 1 :: lookup (right) (c)
        }
        text flatMap lookup(tree)
    }
    
    type CodeTable = List[(Char, List[Bit])]
    
    def codeBits(table: CodeTable)(char: Char): List[Bit] = ???
    
    def convert(tree: CodeTree): CodeTable = ???
    
    def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = ???
    
    def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = ???
    
    
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    combine(leaflist)
    
    List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4))
    
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
    
    encode(t1)("ab".toList)
    
    decode(t1, encode(t1)("ab".toList))
}
