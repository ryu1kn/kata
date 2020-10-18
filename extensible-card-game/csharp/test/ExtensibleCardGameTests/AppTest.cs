using System;
using Xunit;
using ExtensibleCardGame;

namespace ExtensibleCardGameTests
{
    public class AppTest
    {
        [Fact]
        public void SimplestCase()
        {
            Assert.Equal(4, App.EvaluateHand("1H,1S,1C,1D"));
        }

        [Fact]
        public void SimpleCase2()
        {
            Assert.Equal(5, App.EvaluateHand("2H,1S,1C,1D"));
        }

        [Fact]
        public void InvalidCard()
        {
            Assert.Equal(0, App.EvaluateHand("14H,1S,1C,1D"));
        }

        [Fact]
        public void UnknownRule()
        {
            Assert.Equal(0, App.EvaluateHand("unknown;1H,1S,1C,1D"));
        }
    }
}
