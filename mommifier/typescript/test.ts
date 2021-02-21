import { assertEquals } from 'https://deno.land/std@0.87.0/testing/asserts.ts';
import { mommify } from './app.ts';

Deno.test('it returns a given string', () => {
    assertEquals(mommify('b'), 'b');
});

Deno.test('it converts a vowel into mommy', () => {
    assertEquals(mommify('a'), 'mommy');
    assertEquals(mommify('e'), 'mommy');
    assertEquals(mommify('i'), 'mommy');
    assertEquals(mommify('o'), 'mommy');
    assertEquals(mommify('u'), 'mommy');
});

Deno.test('it converts only a vowel into mommy', () => {
    assertEquals(mommify('bba'), 'bbmommy');
});

Deno.test("it doesn't convert a vowel if it's no more than 30%", () => {
    assertEquals(mommify('bbba'), 'bbba');
});

Deno.test("it converts consecutive vowels to one mommy", () => {
    assertEquals(mommify('aa'), 'mommy');
});
