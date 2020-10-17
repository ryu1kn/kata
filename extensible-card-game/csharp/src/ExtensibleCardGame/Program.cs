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
        public static Func<string, int> EvaluateHand = hand => {
            try {
                return hand.Split(',')
                    .Select(card => Card.From(card).Value)
                    .Sum();
            } catch (InvalidCardException) {
                return 0;
            }
        };
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
