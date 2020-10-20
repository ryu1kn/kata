using System;
using System.Linq;

namespace ExtensibleCardGame
{
    public readonly struct Card
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
}
