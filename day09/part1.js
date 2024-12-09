const fs = require('fs');

const raw = fs.readFileSync('./input.txt', 'utf8')

const logIfVerboseEnv = (...args) => {
  if (process.env.VERBOSE) {
    console.log(...args);
    }
};

logIfVerboseEnv('raw:', raw);

const parts = raw.split('').map(Number);
const pushNumber = (queue, number, amount) => {
    for (let i = 0; i < amount; i++) {
        queue.push(number);
    }
};
const takeFromQueue = (src, target, amount) => {
    for (let i = 0; i < amount; i++) {
        target.push(src.shift());
    }
}

let i = 0;
let id = 0;
let rId = Math.ceil(parts.length / 2) - 1;
let l = 0;
let r = parts.length - 1;

let part1 = 0;
let leftQueue = [];
let rightQueue = [];
let blank = false;
const valRow = [];
while (true) {
    if (leftQueue.length === 0 && l <= r) {
        const blockSize = parts[l++]
        if (!blank) {
            pushNumber(leftQueue, id++, blockSize);
        } else {
            while (rightQueue.length < blockSize) {
                let rBlock = parts[r];
                r = r - 2;
                pushNumber(rightQueue, rId--, rBlock);
            }
            takeFromQueue(rightQueue, leftQueue, blockSize);
        }
        blank = !blank;
        if (leftQueue.length === 0) {
            continue;
        }
    }
    if (leftQueue.length === 0) {
        if (rightQueue.length > 0) {
            takeFromQueue(rightQueue, leftQueue, rightQueue.length);
        } else {
            break;
        }
    }
    const val = leftQueue.shift();
    if (process.env.VERBOSE) {
        valRow.push(val);
    }
    part1 += (i * val);
    logIfVerboseEnv('sum:', part1);
    i++;
}

logIfVerboseEnv('valRow:', valRow.join(''));

console.log(JSON.stringify({ l, r, i, id }, null, 2));
console.log('Part1: ', part1);
