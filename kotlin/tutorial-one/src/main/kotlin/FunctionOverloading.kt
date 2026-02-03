class Calculator {

    fun add(a: Int, b: Int): Int {
        return a + b
    }

    fun add(a: Double, b: Double): Int {
        return (a + b).toInt()
    }

    fun add(a: Int, b: Int, c: Int): Int {
        return a + b + c
    }

    fun addList(nums: List<Int>): Int {
        return nums.sum()
    }

    fun addList(nums: List<Double>): Int {
        return nums.sum().toInt()
    }

}

fun main() {
    val calc = Calculator()

    println(calc.add(2, 3))        // calls add(Int, Int)
    println(calc.add(2.5, 4.2))    // calls add(Double, Double)
    println(calc.add(1, 2, 3))     // calls add(Int, Int, Int)

//
//    println(calc.addList(listOf(1,2,3)))
    /* list plus an element */

    val myList = listOf(1,2, "String")
}
