import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class TwoSumTest {
    @Test
    fun testCaseOne() {
        val result = twoSum(
            nums = listOf(2,7,11,15),
            target = 9
        )

        assertEquals(result, listOf(0, 1))
    }

    @Test
    fun testCaseTwo() {
        val result = twoSum(
            nums = listOf(3,2,4),
            target = 6
        )

        assertEquals(result, listOf(1, 2))
    }

    @Test
    fun testCaseThree() {
        val result = twoSum(
            nums = listOf(3,3),
            target = 6
        )

        assertEquals(result, listOf(0, 1))
    }
}