fun main() {
    val myPost = Post("12e2", "ertfhg")

    myPost.also {  }
}


/* Apply example
*
* pretend that you're inside the body of the class
*/
data class Post(val title:String, val body: String)

fun createPost(title:String?, body: String?) {

}

fun postToDataBase(post: Post) {
    println(post);
}

/**
 * when is similar but allows a return value
 */


/**
 * let is similar but with a lambda...
 */

fun letExample() {

}

/**
 * also is let but with no return value
 */