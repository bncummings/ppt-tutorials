
fun main() {
    doubleBang(null)
}

/**
 * Don't use double bang unless you're absolutely certain that it's not null
 */
fun doubleBang(arg: String?) {
}

/**
 * Example:
 */
fun safeDoubleBang(arg: String?) {
    if(arg != null) {
        prettyPrint(arg)
    }
}

fun prettyPrint(str: String) {
    println("~$str~")
}
