const { log } = require('console');
const fs = require('fs');
const { get } = require('http');

const raw = fs.readFileSync('./input.txt', 'utf8')

const logIfVerboseEnv = (...args) => {
  if (process.env.VERBOSE) {
    console.log(...args);
    }
};

logIfVerboseEnv('raw:\n', raw);

const regex = /\b\d+\b/g;

const parseNumbersFromChunk = (chunk) => chunk.match(regex).map(Number);

const equations = raw.split('\n\n').map(parseNumbersFromChunk);
logIfVerboseEnv('equations:\n', equations);

const solveEquation = ([x1, y1, x2, y2, rX, rY]) => {
    const det = x1 * y2 - x2 * y1;
    const a = (y2 * rX - x2 * rY) / det;
    if (a < 0 || a % 1 !== 0) {
        return [0, 0];
    }
    const b = (-y1 * rX + x1 * rY) / det;
    if (b < 0 || b % 1 !== 0) {
        return [0, 0];
    }
    return [a, b];
};


let part1 = equations.map(solveEquation).map(([a,b]) => 3*a + b).reduce((acc, val) => acc + val, 0);
console.log('Part1: ', part1);

const offset = 10000000000000;
let part2 = equations.map(([x1, y1, x2, y2, rX, rY]) => ([x1, y1, x2, y2, offset + rX, offset + rY])).map(solveEquation).map(([a,b]) => 3*a + b).reduce((acc, val) => acc + val, 0);
console.log('Part2: ', part2);
