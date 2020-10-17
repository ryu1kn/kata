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
                    .Select(CardValue)
                    .Sum();
            } catch {
                return 0;
            }
        };

        private static Func<string, int> CardValue =
            card => {
                var value = int.Parse(card.Remove(card.Length - 1));
                if (value > 13) throw new Exception();
                return value;
            };
    }
}
