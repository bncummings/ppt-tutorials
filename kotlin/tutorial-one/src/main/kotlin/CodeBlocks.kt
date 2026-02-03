/* what's wrong with this? */
//
//private fun transform(num: Int): Int {
//    return num + 1
//}
//
//private val intTransForm: (Int) -> Int = {
//    num -> num + 1
//}

//private val intTransform: (Int, Int) -> Int = {
//
//}

fun main() {
    val myList = listOf("1,2,3")
    println()
    println(myList.map{it + 1})

    myList.filter(String::isNullOrBlank)
}
