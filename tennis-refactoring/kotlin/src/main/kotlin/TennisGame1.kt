import TennisGame1.Point.Companion.resolveLabel

class TennisGame1(private val player1Name: String, private val player2Name: String) : TennisGame {

    enum class Point(val value: Int, val label: String) {
        LOVE(0, "Love"),
        FIFTEEN(1, "Fifteen"),
        THIRTY(2, "Thirty"),
        FORTY(3, "Forty"),
        SIXTY(4, "Sixty");

        operator fun compareTo(point: Int): Int = this.value - point

        companion object {
            fun resolveLabel(point: Int): String = Point.values()[point].label
        }
    }

    private val DEUCE = "Deuce"
    private var m_score1: Int = 0
    private var m_score2: Int = 0

    override fun wonPoint(playerName: String) {
        if (playerName === player1Name)
            m_score1 += 1
        else
            m_score2 += 1
    }

    override fun getScore(): String {
        return if (isInEarlyStage()) earlyStageScores() else laterStageScores()
    }

    private fun isInEarlyStage(): Boolean {
        return Point.SIXTY > m_score1 && Point.SIXTY > m_score2
    }

    private fun earlyStageScores(): String {
        return if (m_score1 == m_score2) sameScore() else nonSameScore()
    }

    private fun sameScore(): String {
        return if (Point.FORTY > m_score1) "${resolveLabel(m_score1)}-All" else DEUCE
    }

    private fun nonSameScore(): String {
        return "${resolveLabel(m_score1)}-${resolveLabel(m_score2)}"
    }

    private fun laterStageScores(): String {
        when (Math.abs(m_score1 - m_score2)) {
            0 -> return DEUCE
            1 -> return "Advantage ${winningPlayer()}"
            else -> return "Win for ${winningPlayer()}"
        }
    }

    private fun winningPlayer(): String {
        return if (m_score1 > m_score2) player1Name else player2Name
    }

}
