package mommifier.kotlin

import mommifier.kotlin.Mommifier.mommify
import kotlin.test.Test
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

    @org.junit.Test
    fun `returns "mommy" if "a" is given`() {
        assertEquals("mommy", mommify("a"))
    }

    @org.junit.Test
    fun `returns "mommy" if "e" is given`() {
        assertEquals("mommy", mommify("e"))
    }
}

