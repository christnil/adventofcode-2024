const fs = require('fs');

const raw = fs.readFileSync('./input.txt', 'utf8')
const [static, gatesInput] = raw.split('\n\n')

const logIfVerboseEnv = (...args) => {
  if (process.env.VERBOSE) {
    console.log(...args);
    }
};

const values = {};

static.split('\n').forEach((line) => {
    const [key, valueString] = line.split(': ');
    values[key] = valueString === '1';
});

const gates = gatesInput.split('\n').map((line) => {
    const [input, output] = line.split(' -> ');
    const [x, op, y] = input.split(' ');
    return { x, y, op, output, done: false };
});

const part1 = () => {
    while (gates.some((gate) => !gate.done)) {
        gates.forEach((gate) => {
            if (gate.done) {
                return;
            }
            if (values[gate.x] !== true && values[gate.x] !== false) {
                return;
            }
            const x = values[gate.x];
            const y = values[gate.y];
            if (x === undefined || y === undefined) {
                return;
            }
            switch (gate.op) {
                case 'AND':
                    values[gate.output] = x && y;
                    break;
                case 'OR':
                    values[gate.output] = x || y;
                    break;
                case 'XOR':
                    values[gate.output] = x !== y;
                    break;
                default:
                    throw new Error('Unknown op ' + gate.op);
            }
            gate.done = true;
        });
    }

    const outPutBinearyString = Object.keys(values)
        .filter((key) => key.startsWith('z'))
        .sort()
        .reverse()
        .map((key) => values[key])
        .map((val) => (val ? '1' : '0'))
        .join('')

    const outPutDecimal = parseInt(outPutBinearyString, 2);

    return outPutDecimal;
};

console.log('Part 1: ', part1())
