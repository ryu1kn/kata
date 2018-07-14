import org.junit.Assert.assertEquals
import org.junit.Test

class MinesweeperTest {

    val minesweeper = Minesweeper()

    @Test
    fun emptyField() {
        val input = emptyList<List<Char>>()
        assertEquals(emptyList<List<Char>>(), minesweeper.fillNumbers(input))
    }

    @Test
    fun fieldWithNoBombs() {
        val input = listOf(listOf('.'))
        assertEquals(listOf(listOf('.')), minesweeper.fillNumbers(input))
    }

}
