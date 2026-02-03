
fun main () {
    val myList: List<String> = listOf(
        "",
        " ",
        "\t",
    )

    println(myList.map(String::isNullOrBlank))
}
