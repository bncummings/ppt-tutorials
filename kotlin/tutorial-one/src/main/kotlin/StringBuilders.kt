data class Person(
    val name: String,
    val age: Int,
    val email: String
) {

    private val secret = 12234

//    override fun toString(): String =
//        "Person(\n" +
//                "    name = \"$name\",\n" +
//                "    age = $age,\n" +
//                "    email = \"$email\"\n" +
//                ")"

//    override fun toString(): String {
//        val sb = StringBuilder()
//        sb.appendLine("Person(")
//        sb.appendLine("    name = \"$name\",")
//        sb.appendLine("    age = $age,")
//        sb.appendLine("    email = \"$email\"")
//        sb.append(")")   // no newline at the end
//        return sb.toString()
//    }

//    override fun toString(): String = buildString {
//        appendLine("Person(")
//        appendLine("    name = \"$name\",")
//        appendLine("    age = $age,")
//        appendLine("    email = \"$email\"")
//        append(")")
//    }

//    override fun toString(): String {
//        val sb = StringBuilder()
//
//        with (sb) {
//            appendLine("hello")
//        }
//
//        return sb.toString()
//    }

    override fun toString(): String = """
        Person(
            name = "$name",
            age = $age,
            email = "$email"
        )
    """.trimIndent()
}


fun main(args: Array<String>) {
    val person = Person("Ben", 21, "example.email@domain.com")
    println(person)
}


/*
* explore using with() as well
*
* */