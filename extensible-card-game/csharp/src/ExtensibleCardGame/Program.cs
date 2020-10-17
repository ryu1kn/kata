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
        public static Func<string, int> EvaluateHand = hand =>
            hand.Split(',')
                .Select(CardValue)
                .Sum();

        private static Func<string, int> CardValue =
            card => int.Parse(card.Remove(card.Length - 1));
    }
}
