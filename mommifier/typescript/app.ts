
const isVowel = (c: string) => 'aeiou'.includes(c)
const lastChar = (chars: string[]) => chars[chars.length - 1]
const processAsChars = (s: string, processor: (chars: string[]) => string[]) =>
    processor(s.split('')).join('')

const shouldMommify = (chars: string[]) => chars.filter(isVowel).length / chars.length > 0.3

const collapseVowels = (chars: string[]) =>
    chars.reduce(
        (result: string[], char) => isVowel(lastChar(result)) ? result : [...result, char],
        []
    )

const doMommify = (chars: string[]) =>
    collapseVowels(chars).map(s => isVowel(s) ? 'mommy' : s)

const mommifyChars = (chars: string[]) =>
    shouldMommify(chars) ? doMommify(chars) : chars

export const mommify = (s: string) => processAsChars(s, mommifyChars)
