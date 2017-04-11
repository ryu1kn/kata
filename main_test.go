package main

import "testing"

func TestAllOne(t *testing.T) {
	game := NewGame()
	rolls := []int{1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}

	actual := roll(game, &rolls)
	if expected := 20; actual != expected {
		t.Fatalf("Expected %d, but got %d", expected, actual)
	}
}

func TestOneSpare(t *testing.T) {
	game := NewGame()
	rolls := []int{5, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

	actual := roll(game, &rolls)
	if expected := 16; actual != expected {
		t.Fatalf("Expected %d, but got %d", expected, actual)
	}
}

func TestTwoSpares(t *testing.T) {
	game := NewGame()
	rolls := []int{5, 5, 3, 7, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

	actual := roll(game, &rolls)
	if expected := 27; actual != expected {
		t.Fatalf("Expected %d, but got %d", expected, actual)
	}
}

func TestOneStrike(t *testing.T) {
	game := NewGame()
	rolls := []int{10, 3, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}

	actual := roll(game, &rolls)
	if expected := 24; actual != expected {
		t.Fatalf("Expected %d, but got %d", expected, actual)
	}
}

func TestPerfectGame(t *testing.T) {
	game := NewGame()
	rolls := []int{10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10}

	actual := roll(game, &rolls)
	if expected := 300; actual != expected {
		t.Fatalf("Expected %d, but got %d", expected, actual)
	}
}

func roll(game *game, rolls *[]int) int {
	for _, roll := range *rolls {
		game.Roll(roll)
	}
	return game.Score()
}
