using System;
using System.Collections.Generic;
using System.Linq;
using ExtensibleCardGame.Rule;

namespace ExtensibleCardGame
{
    internal static class Program
    {
        private static void Main(string[] _args)
        {
            Console.WriteLine("Hello World!");
        }
    }

    public static class App
    {
        public static readonly Func<string, int> EvaluateHand = game => {
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

    internal class Game
    {
        private readonly string _customRule;
        private readonly List<Card> _cards;
        private readonly List<IGameRule> _rules = new List<IGameRule>
        {
            new NoCustomRule(),
            new PreferOddRule(),
            new SameSuiteRule()
        };

        public Game(string game)
        {
            var parts = game.Split(';');
            if (parts.Length > 1)
            {
                _customRule = parts[0];
                _cards = ToCards(parts[1]);
            }
            else
            {
                _customRule = "";
                _cards = ToCards(parts[0]);
            }
        }

        private static List<Card> ToCards(string cards) =>
            cards.Split(',').Select(Card.From).ToList();

        public int Evaluate() => _rules.Find(rule => rule.Match(_customRule))?.Point(_cards) ?? 0;
    }

    internal class InvalidCardException : Exception { }
}
