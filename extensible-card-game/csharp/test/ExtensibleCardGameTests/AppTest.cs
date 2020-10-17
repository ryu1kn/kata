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
    }
}
