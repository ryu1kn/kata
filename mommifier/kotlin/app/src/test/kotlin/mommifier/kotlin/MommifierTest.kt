package mommifier.kotlin

import mommifier.kotlin.Mommifier.mommify
import org.junit.Test
import kotlin.test.assertEquals

class MommifierTest {
    @Test
    fun `returns an empty string`() {
        assertEquals("", mommify(""))
    }

    @Test
    fun `returns a consonant`() {
        assertEquals("b", mommify("b"))
    }

    @Test
    fun `returns "mommy" if a vowel is given`() {
        assertEquals("mommy", mommify("a"))
        assertEquals("mommy", mommify("e"))
        assertEquals("mommy", mommify("i"))
        assertEquals("mommy", mommify("o"))
        assertEquals("mommy", mommify("u"))
    }

    @Test
    fun `replaces only a vowel in a string`() {
        assertEquals("mommyb", mommify("ab"))
    }

    @Test
    fun `replaces consecutive vowels with one mommy`() {
        assertEquals("mommy", mommify("aa"))
    }
}

