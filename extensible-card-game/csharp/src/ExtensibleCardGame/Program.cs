using System;
using System.Collections.Generic;
using System.Linq;
using ExtensibleCardGame.Rule;

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
        private readonly List<IGameRule> rules = new List<IGameRule>
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

        public int Evaluate() => rules.Find(rule => rule.Match(customRule))?.Point(cards) ?? 0;
    }

    class InvalidCardException : Exception { }
}
