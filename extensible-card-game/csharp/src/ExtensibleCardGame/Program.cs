using System;
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
            try {
                return new Game(game).Evaluate();
            } catch (InvalidCardException) {
                return 0;
            }
        };
    }

    class Game {
        private readonly string rule;
        private readonly string cards;

        public Game(string game)
        {
            var parts = game.Split(';');
            if (parts.Length > 1) {
                rule = parts[0];
                cards = parts[1];
            } else {
                rule = "";
                cards = parts[0];
            }
        }

        public int Evaluate()
        {
            if (rule != "") return 30;
            return cards
                .Split(',')
                .Select(card => Card.From(card).Value)
                .Sum();
        }
    }

    readonly struct Card {
        public readonly int Value;

        Card(int value) {
            this.Value = value;
        }

        static public Func<string, Card> From = card => {
            var value = int.Parse(card.Remove(card.Length - 1));
            if (value > 13) throw new InvalidCardException();
            return new Card(value);
        };
    }

    class InvalidCardException : Exception {}
}
