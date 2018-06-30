const CAGE_PLACE_HOLDER = '_';

exports.nextMorning = farm => {
  const cages = extractMatches(farm, /\[(.*?)\]/g)
  const cagesNextMorning = cages.map(stayNight)

  const outside = farm.replace(/\[.*?\]/g, CAGE_PLACE_HOLDER)
  const outsideSectionsNextMorning = stayNight(outside).split(CAGE_PLACE_HOLDER)

  return concatenateSections(outsideSectionsNextMorning, cagesNextMorning)
}

function extractMatches(string, pattern) {
  const match = string.match(pattern);
  return match ? [...match] : [];
}

function stayNight(section) {
  return section.includes('F') ? section.replace(/C/g, '.') : section
}

function concatenateSections(outsideSections, cages) {
  return cages.reduce((left, cage, position) =>
    left + cage + outsideSections[position + 1],
    outsideSections[0]
  )
}
