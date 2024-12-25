const fs = require('fs');

const raw = fs.readFileSync('./input.txt', 'utf8')
const rows = raw.split('\n')


const logIfVerboseEnv = (...args) => {
  if (process.env.VERBOSE) {
    console.log(...args);
    }
};

const evolveSecretNumber = (secretNumber) => {
    const MODULO = 16777216;

    secretNumber ^= (secretNumber * 64);
    secretNumber = ((secretNumber % MODULO) + MODULO) % MODULO;

    const divisionResult = Math.floor(secretNumber / 32);
    secretNumber ^= divisionResult;
    secretNumber = ((secretNumber % MODULO) + MODULO) % MODULO;

    secretNumber ^= (secretNumber * 2048);
    secretNumber = ((secretNumber % MODULO) + MODULO) % MODULO;

    return secretNumber;
}

const getLastDigit = (number) => number % 10;
const getDiff = (prev, curr) => curr - prev;

const getSeqCache = (number) => {
    const cache = {};
    let res = number;
    const lastFourDiffs = [];
    for (let i = 0; i < 2000; i++) {
        const next = evolveSecretNumber(res);
        const diff = getDiff(getLastDigit(res), getLastDigit(next));
        lastFourDiffs.push(diff);
        if (lastFourDiffs.length > 4) {
            lastFourDiffs.shift();
            const key = lastFourDiffs.join(',');
            if (!cache[key]) {
                cache[key] = getLastDigit(next);
            }
        }
        res = next
    }
    return cache;
}

const valueByKey = rows.map(row => parseInt(row)).map(getSeqCache).reduce((acc, curr) => {
    Object.keys(curr).forEach(key => {
        if (!acc[key]) {
            acc[key] = curr[key];
        } else {
            acc[key] += curr[key];
        }
    });
    return acc;
}, {});

const findKeyWithHighestValue = (obj) => {
    return Object.keys(obj).reduce((highestKey, key) => 
        (!obj[highestKey] || (obj[key] > obj[highestKey])) ? key : highestKey
    , '');
}

const key = findKeyWithHighestValue(valueByKey);
const part1 = valueByKey[key];
console.log('Best key:', key)
console.log('Part 2 value: ', part1)
