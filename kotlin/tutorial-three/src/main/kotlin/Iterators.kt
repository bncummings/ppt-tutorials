fun main() {
//    val nums = arrayListOf(1, 2, 3, 3)
//    removeEvenNumbers(nums)
}

///**
// * What happens here?
// */
//fun removeEvenNumbers(nums: ArrayList<Int>){
//    for (n in nums) {
//        if (n % 2 == 0) {
//            nums.remove(n)
//        }
//    }
//    println(nums)
//}

fun removeEvenNumbers(nums: ArrayList<Int>) {
    val iterator = nums.iterator()

    while(iterator.hasNext()) {
        if(iterator.next() % 2 == 0) iterator.remove()
    }

    println(nums)
}



