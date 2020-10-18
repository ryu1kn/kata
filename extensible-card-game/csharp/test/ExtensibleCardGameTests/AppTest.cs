using Xunit;
using ExtensibleCardGame;

namespace ExtensibleCardGameTests
{
    public class AppTest
    {
        [Fact]
        public void SimplestCase() =>
            Assert.Equal(4, App.EvaluateHand("1H,1S,1C,1D"));

        [Fact]
        public void SimpleCase2() =>
            Assert.Equal(5, App.EvaluateHand("2H,1S,1C,1D"));

        [Fact]
        public void InvalidCard() =>
            Assert.Equal(0, App.EvaluateHand("14H,1S,1C,1D"));

        [Fact]
        public void UnknownRule() =>
            Assert.Equal(0, App.EvaluateHand("unknown;1H,1S,1C,1D"));

        [Fact]
        public void PreferOddRule() =>
            Assert.Equal(14, App.EvaluateHand("prefer-odd;1H,2S,3C,4D"));

        [Fact]
        public void SameSuiteRule() =>
            Assert.Equal(60, App.EvaluateHand("same-suite;1H,2H,3H,4H"));
    }
}
