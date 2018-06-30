
const {JSDOM} = require('jsdom')

JSDOM.fromURL('http://romannumerals.babuo.com/roman-numerals-1-5000')
  .then(dom => {
    const elems = [...dom.window.document.querySelectorAll('#web > ul > li')]
    const arabicRomanPairs = elems.map(extractArabicRomanPair).filter(pair => pair.arabic < 4000)
    console.log(buildJson(arabicRomanPairs))
  })

function extractArabicRomanPair(domElement) {
  const match = domElement.textContent.match(/^(\d+): *(.*)$/)
  return {
    arabic: Number(match[1]),
    roman: match[2]
  }
}

function buildJson(arabicRomanPairs) {
  return JSON.stringify(arabicRomanPairs, null, 2)
}

