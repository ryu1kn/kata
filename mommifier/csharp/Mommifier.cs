using System.Collections.Generic;
using System.Linq;

namespace Mommifier
{
    using static MommifierExtension;

    public class Mommifier
    {
        public string Mommify(string s) => ShouldMommify(s) ? DoMommify(s) : s;

        private string DoMommify(string s) => SquashVowels(s).Select(Mommify).Join();

        private bool ShouldMommify(string s) => GetVowelRatio(s) > 0.3;

        private double GetVowelRatio(string s) =>
            (double)s.Where(MommifierExtension.IsVowel).Count() / s.Length;

        private string Mommify(char c) => c.IsVowel() ? "mommy" : c.ToString();

        private string SquashVowels(string s) =>
            s.Aggregate("",
                (sub, next) => sub switch
                {
                    "" => next.ToString(),
                    _  => sub.EndWithVowel() && next.IsVowel() ? sub : sub + next,
                }
            );
    }

    internal static class MommifierExtension
    {
        public static string Join<T>(this IEnumerable<T> objects) =>
            string.Join("", objects.Select(o => o.ToString()));

        public static bool EndWithVowel(this string s) => s != "" && IsVowel(s.Last());

        public static bool IsVowel(this char c) => "aeiou".Contains(c);
    }
}
