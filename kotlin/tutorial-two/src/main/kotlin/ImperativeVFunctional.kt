fun main() {
    val words = listOf<String?>("Example", "AnotherExample")
    val scores = mutableListOf<Int>(0, 0, 0, 0, 0)
    val letters = listOf<Char>(
        'a',
        'e',
        'i',
        'l',
        'n',
        'o',
        'r',
        's',
        't',
        'u',
        'd',
        'g',
        'b',
        'c',
        'm',
        'p',
        'f',
        'h',
        'v',
        'w',
        'y',
        'k',
        'j',
        'x',
        'q',
        'z'
    )

    words.forEachIndexed{
        index, word ->
    }

    for (w in words) {
        val index = words.indexOf(w)
        if (w != null) {
            for (c in w) {
                val i = letters.indexOf(c)
                if (i != -1) {
                    scores[index] += when {
                        i < 10 -> 1
                        i < 12 -> 2
                        i < 16 -> 3
                        i < 21 -> 4
                        i < 22 -> 5
                        i < 24 -> 8
                        else -> 10
                    }
                }
            }
        }
    }

    println(words.zip(scores))
}