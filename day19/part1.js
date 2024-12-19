const { log } = require('console');
const fs = require('fs');

const raw = fs.readFileSync('./input.txt', 'utf8')
const [patternInput, designInput] = raw.split('\n\n')


const logIfVerboseEnv = (...args) => {
  if (process.env.VERBOSE) {
    console.log(...args);
    }
};

const patternsByFirstLetter = patternInput.split(', ').reduce((acc, word) => {
    const firstLetter = word[0];
    acc[firstLetter] = acc[firstLetter] || [];
    acc[firstLetter].push(word);
    return acc;
}, {});

const designs = designInput.split('\n');

const possibleDesignCache = {};
const isPossibleDesign = (design) => {
    if (possibleDesignCache[design] !== undefined) {
        return possibleDesignCache[design];
    }
    let result = false;
    if (design.length === 0) {
        return result = true;
    } else {
        let firstLetter = design[0];
        let possiblePatterns = patternsByFirstLetter[firstLetter];
        if (possiblePatterns) {
            for (let pattern of possiblePatterns) {
                if (design.startsWith(pattern) && isPossibleDesign(design.slice(pattern.length))) {
                    result = true;
                    break;
                }
            }
        }
    }
    possibleDesignCache[design] = result;
    return result;
};

const possilbeDesigns = designs.filter(isPossibleDesign)

logIfVerboseEnv("possilbeDesigns", possilbeDesigns);

console.log('Part 1: ', possilbeDesigns.length);

const cache = {};
const numberOfPossibleCombinationsForDesign = (design) => {
    if (cache[design]) {
        return cache[design];
    }
    if (design.length === 0) {
        return 1;
    }
    let firstLetter = design[0];
    let possiblePatterns = patternsByFirstLetter[firstLetter];
    if (!possiblePatterns) {
        return 0;
    }
    let count = 0;
    for (let pattern of possiblePatterns) {
        if (design.startsWith(pattern)) {
            count += numberOfPossibleCombinationsForDesign(design.slice(pattern.length));
        }
    }
    cache[design] = count;
    return count;
}

const part2 = designs.map(numberOfPossibleCombinationsForDesign).reduce((acc, count) => acc + count, 0);

console.log('Part 2: ', part2);
