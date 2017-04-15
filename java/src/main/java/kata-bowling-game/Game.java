
package kata_bowling_game;

public class Game {

    public static final int MAX_PINS_IN_FRAME = 10;
    private static final int MAX_FLAME = MAX_PINS_IN_FRAME;
    private int rolls[] = new int[21];
    private int currentRoll = 0;

    Game() {
    }

    public void roll(int pins) {
        rolls[currentRoll++] = pins;
    }

    public int score() {
        int score = 0;
        int rollIndex = 0;
        for (int frame = 0; frame < MAX_FLAME; frame++) {
            if (isStrike(rollIndex)) {
                score += MAX_PINS_IN_FRAME + strikeBonus(rollIndex);
                rollIndex += 1;
            } else if (isSpare(rollIndex)) {
                score += MAX_PINS_IN_FRAME + spareBonus(rollIndex);
                rollIndex += 2;
            } else {
                score += sumOfPinsInFrame(rollIndex);
                rollIndex += 2;
            }
        }
        return score;
    }

    private int spareBonus(int rollIndex) {
        return rolls[rollIndex + 2];
    }

    private int strikeBonus(int rollIndex) {
        return rolls[rollIndex + 1] + rolls[rollIndex + 2];
    }

    private int sumOfPinsInFrame(int rollIndex) {
        return rolls[rollIndex] + rolls[rollIndex+1];
    }

    private boolean isStrike(int rollIndex) {
        return rolls[rollIndex] == MAX_PINS_IN_FRAME;
    }

    private boolean isSpare(int rollIndex) {
        return sumOfPinsInFrame(rollIndex) == MAX_PINS_IN_FRAME;
    }

}
