package mommifier.kotlin

import kotlin.test.Test
import kotlin.test.assertEquals

class AppTest {
    @Test fun appHasAGreeting() {
        val classUnderTest = App()
        assertEquals("Hello World!", classUnderTest.greeting)
    }
}
