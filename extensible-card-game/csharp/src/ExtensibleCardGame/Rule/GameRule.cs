using System.Collections.Generic;
using System.Linq;
using ExtensibleCardGame.Extension;

namespace ExtensibleCardGame.Rule
{
    public interface IGameRule
    {
        public bool Match(string ruleId) => ruleId == RuleName;

        protected string RuleName { get; }

        public int Point(IEnumerable<Card> cards);
    }

    public class NoCustomRule : IGameRule
    {
        string IGameRule.RuleName => "";

        public int Point(IEnumerable<Card> cards) =>
            cards.Select(c => c.Value).Sum();
    }

    public class PreferOddRule : IGameRule
    {
        string IGameRule.RuleName => "prefer-odd";

        public int Point(IEnumerable<Card> cards) =>
            cards.Select(c => c.Value + (c.Value % 2 == 0 ? 0 : 2)).Sum();
    }

    public class SameSuiteRule : IGameRule
    {
        string IGameRule.RuleName => "same-suite";

        public int Point(IEnumerable<Card> cards) =>
            cards.Select(c => c.Value).Sum()
            + (cards.CountBy(c => c.Suite) == 1 ? 50 : 0);
    }
}
