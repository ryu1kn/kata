using System;
using System.Collections.Generic;
using System.Linq;

namespace ExtensibleCardGame
{
    class Program
    {
        static void Main(string[] args)
        {
            Console.WriteLine("Hello World!");
        }
    }

    public class App
    {
        public static Func<string, int> EvaluateHand = game => {
            try
            {
                return new Game(game).Evaluate();
            }
            catch (InvalidCardException)
            {
                return 0;
            }
        };
    }

    class Game
    {
        private readonly string customRule;
        private readonly List<Card> cards;

        public Game(string game)
        {
            var parts = game.Split(';');
            if (parts.Length > 1)
            {
                customRule = parts[0];
                cards = ToCards(parts[1]);
            }
            else
            {
                customRule = "";
                cards = ToCards(parts[0]);
            }
        }

        private List<Card> ToCards(string cards) =>
            cards.Split(',').Select(Card.From).ToList();

        public int Evaluate() => customRule switch
        {
            "" => cards.Select(c => c.Value).Sum(),
            "prefer-odd" => cards.Select(c => c.Value + (c.Value % 2 == 0 ? 0 : 2)).Sum(),
            "same-suite" => cards.Select(c => c.Value).Sum() + (cards.Select(c => c.Suite).Distinct().Count() == 1 ? 50 : 0),
            _ => 0
        };
    }

    readonly struct Card
    {
        public readonly int Value;
        public readonly char Suite;

        Card(int value, char suite)
        {
            Value = value;
            Suite = suite;
        }

        public static Func<string, Card> From = card =>
        {
            var value = int.Parse(card.Remove(card.Length - 1));
            return value > 13 ? throw new InvalidCardException() : new Card(value, card.Last());
        };
    }

    class InvalidCardException : Exception { }
}
