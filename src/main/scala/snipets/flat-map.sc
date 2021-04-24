val coll = Seq('a', 'b', 'c', 'd')

def operation(a: Char) = s"${a}b".toCharArray

val res = coll.flatMap(operation)
