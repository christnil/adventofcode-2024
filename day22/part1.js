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

const getNumber2000From = (number) => {
    let res = number;
    for (let i = 0; i < 2000; i++) {
        res = evolveSecretNumber(res);
    }
    return res;
}

const part1 = rows.map(row => parseInt(row)).map(getNumber2000From).reduce((acc, curr) => {
    console.log('curr', curr)
    return acc + curr
}, 0);

console.log('Part 1: ', part1)
