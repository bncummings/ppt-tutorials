class BenList<T>(private val elems: List<T>): Iterable<T>{

    override fun iterator(): Iterator<T> {
        return BensIterator()
    }

    inner class BensIterator():Iterator<T> {
        var index = 0

        override fun hasNext(): Boolean {
            return index < elems.size
        }

        override fun next(): T {
            return elems[index++]
        }
    }

}

fun main() {
    val myBenList: BenList<Int> = BenList(listOf(1,2,3))

    for(num in myBenList) {
        println(num)
    }
}