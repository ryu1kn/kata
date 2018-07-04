import TennisGame1.Point.Companion.resolveLabel

class TennisGame1(private val player1Name: String, private val player2Name: String) : TennisGame {

    enum class Point(val value: Int, val label: String) {
        LOVE(0, "Love"),
        FIFTEEN(1, "Fifteen"),
        THIRTY(2, "Thirty"),
        FORTY(3, "Forty"),
        SIXTY(4, "Sixty");

        operator fun compareTo(point: Int) = this.value - point

        companion object {
            fun resolveLabel(point: Int): String = Point.values()[point].label
        }
    }

    private val DEUCE = "Deuce"
    private var player1Point: Int = 0
    private var player2Point: Int = 0

    override fun wonPoint(playerName: String) {
        if (playerName === player1Name)
            player1Point += 1
        else
            player2Point += 1
    }

    override fun getScore(): String = if (isInEarlyStage()) earlyStageScores() else laterStageScores()

    private fun isInEarlyStage() = Point.SIXTY > player1Point && Point.SIXTY > player2Point

    private fun earlyStageScores() = if (player1Point == player2Point) sameScore() else nonSameScore()

    private fun sameScore() = if (Point.FORTY > player1Point) "${resolveLabel(player1Point)}-All" else DEUCE

    private fun nonSameScore() = "${resolveLabel(player1Point)}-${resolveLabel(player2Point)}"

    private fun laterStageScores() = when (Math.abs(player1Point - player2Point)) {
        0 -> DEUCE
        1 -> "Advantage ${winningPlayer()}"
        else -> "Win for ${winningPlayer()}"
    }

    private fun winningPlayer() = if (player1Point > player2Point) player1Name else player2Name

}
