using System;
using Xunit;

namespace Mommifier
{
    public class UnitTest1
    {
        private Func<string,string> mommify = new Mommifier().Mommify;

        [Fact]
        public void EmptyString() =>
            Assert.Equal("", mommify(""));

        [Fact]
        public void AVowel()
        {
            Assert.Equal("mommy", mommify("a"));
            Assert.Equal("mommy", mommify("e"));
            Assert.Equal("mommy", mommify("i"));
            Assert.Equal("mommy", mommify("o"));
            Assert.Equal("mommy", mommify("u"));
        }

        [Fact]
        public void AVowelWithConsonant() =>
            Assert.Equal("mommyb", mommify("ab"));

        [Fact]
        public void ConsecutiveVowels()
        {
            Assert.Equal("mommy", mommify("aa"));
            Assert.Equal("mommy", mommify("ae"));
        }

        [Fact]
        public void ConsecutiveVowelsAndConsonants()
        {
            Assert.Equal("mommyb", mommify("aab"));
        }

        [Fact]
        public void DontMommifyWhenVowelRatioIsLessThan30()
        {
            Assert.Equal("abbb", mommify("abbb"));
        }
    }
}
