fun main() {
    /* */
    val myString = "Haskell Sucks"
    val score = myString.score()

    println(score)
}

private fun String.score(): Int {
    println(this)
    return this.sumOf { it.code }
}
