package main

func NewGame() *game {
	return &game{
		rollIndex: 0,
		rolls:     make([]int, 21),
	}
}

type game struct {
	rollIndex int
	rolls     []int
}

func (g *game) Roll(pins int) {
	g.rolls[g.rollIndex] = pins
	g.rollIndex++
}

func (g *game) Score() int {
	score := 0
	rollIndex := 0
	for frame := 0; frame < 10; frame++ {
		if g.isStrike(rollIndex) {
			score += 10 + g.strikeBonus(rollIndex)
			rollIndex++
		} else if g.isSpare(rollIndex) {
			score += 10 + g.spareBonus(rollIndex)
			rollIndex += 2
		} else {
			score += g.sumOfBallsInFrame(rollIndex)
			rollIndex += 2
		}
	}
	return score
}

func (g *game) sumOfBallsInFrame(rollIndex int) int {
	return g.rolls[rollIndex] + g.rolls[rollIndex+1]
}

func (g *game) isSpare(rollIndex int) bool {
	return g.rolls[rollIndex]+g.rolls[rollIndex+1] == 10
}

func (g *game) isStrike(rollIndex int) bool {
	return g.rolls[rollIndex] == 10
}

func (g *game) spareBonus(rollIndex int) int {
	return g.rolls[rollIndex+2]
}

func (g *game) strikeBonus(rollIndex int) int {
	return g.rolls[rollIndex+1] + g.rolls[rollIndex+2]
}
