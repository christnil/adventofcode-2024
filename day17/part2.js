const fs = require('fs');
const { parse, join } = require('path');

const raw = fs.readFileSync('./input.txt', 'utf8')

const logIfVerboseEnv = (...args) => {
  if (process.env.VERBOSE) {
    console.log(...args);
    }
};

logIfVerboseEnv('raw:\n', raw);

const [registryInput, programInput] = raw.split('\n\n')

const getAllNumbers = (str) => str.match(/\d+/g).map(Number);

let registry = getAllNumbers(registryInput);
const memory = getAllNumbers(programInput);
let pointer = 0;

const readLitteral = (addr) => memory[addr];
const readRegistry = (addr) => registry[addr];
const readCombo = (addr) => readLitteral(addr) < 4 ? readLitteral(addr) : readRegistry(readLitteral(addr) - 4);
const fakeCombo = (addr) => readLitteral(addr) < 4 ? readLitteral(addr) : fakeRegistry[readLitteral(addr) - 4];
const writeMemory = (addr, value) => memory[addr] = (value & 0b111);
const writeRegistry = (addr, value) => registry[addr] = value;
const writePointer = (value) => pointer = value;
const incPointer = () => pointer += 2;

const fakeRegistry = ['A', 0, 0];
const trySimplify = (val) => {
    if (parseInt(val, 10) === val) {
        return parseInt(val);
    }
    try {
        const res = eval(val);
        if (res !== 0)
            return res;
    } catch (e) {
        return val;
    }
}

let outPut = [];
const fakeOutPut = [];
const writeOutput = (value) => outPut.push(value);

const xor = (a, b) => Number(BigInt(a) ^ BigInt(b));

const adv = (addr) => {
    const n = readRegistry(0);
    const d = 1 << readCombo(addr + 1);
    const res = Math.floor(n / d);
    writeRegistry(0, res);
    logIfVerboseEnv('adv');
    logIfVerboseEnv('n: ', n, ', d: ', d, ', res:', res);
    const str = `Math.floor(${fakeRegistry[0]} / ${d})`;
    fakeRegistry[0] = trySimplify(str);
};

const bxl = (addr) => {
    const a = readRegistry(1);
    const b = readLitteral(addr + 1);
    const res = xor(a, b);
    writeRegistry(1, res);
    logIfVerboseEnv('bxl');
    logIfVerboseEnv('a: ', a, ', b: ', b, ', res:', res);
    const str = `(${fakeRegistry[1]} ^ ${b})`;
    fakeRegistry[1] = trySimplify(str);
};

const bst = (addr) => {
    const a = readCombo(addr + 1);
    const res = a & 0b111;
    writeRegistry(1, res);
    logIfVerboseEnv('bst');
    logIfVerboseEnv('a: ', a, ', res:', res);
    const fakeA = fakeCombo(addr + 1);
    const str = `(${fakeA} & 0b111)`;
    fakeRegistry[1] = trySimplify(str);
};

const jnz = (addr) => {
    const a = readRegistry(0);
    const litteral = readLitteral(addr + 1);
    logIfVerboseEnv('jnz');
    if (a !== 0) {
        logIfVerboseEnv('Juming a: ', a, ', litteral:', litteral);
        writePointer(litteral - 2);
    }
    logIfVerboseEnv('Noop a: ', a, ', litteral:', litteral);
};

const bxc = (addr) => {
    const b = readRegistry(1);
    const c = readRegistry(2);
    const res = xor(b, c);
    writeRegistry(1, res);
    logIfVerboseEnv('bxc');
    logIfVerboseEnv('b: ', b, ', c: ', c, ', res:', res);
    const str = `(${fakeRegistry[1]} ^ ${fakeRegistry[2]})`;
    fakeRegistry[1] = trySimplify(str);
};

const out = (addr) => {
    const a = readCombo(addr + 1);
    const res = a & 0b111;
    writeOutput(res);
    logIfVerboseEnv('out');
    logIfVerboseEnv('a: ', a, ', res:', res);
    const fakeA = fakeCombo(addr + 1);
    const str = `(${fakeA} & 0b111)`;
    fakeOutPut.push(trySimplify(str));
};

const bdv = (addr) => {
    const n = readRegistry(0);
    const d = 1 << readCombo(addr + 1);
    const res = Math.floor(n / d);
    writeRegistry(1, res);
    logIfVerboseEnv('bdv');
    logIfVerboseEnv('n: ', n, ', d: ', d, ', res:', res);
    const fakeD = fakeCombo(addr + 1);
    const str = `Math.floor(${fakeRegistry[0]} / ${fakeD})`;
    fakeRegistry[1] = trySimplify(str);
};

const cdv = (addr) => {
    const n = readRegistry(0);
    const d = 1 << readCombo(addr + 1);
    const res = Math.floor(n / d);
    writeRegistry(2, res);
    logIfVerboseEnv('cdv');
    logIfVerboseEnv('n: ', n, ', d: ', d, ', res:', res);
    const fakeD = fakeCombo(addr + 1);
    const str = `Math.floor(${fakeRegistry[0]} / ${fakeD})`;
    fakeRegistry[2] = trySimplify(str);
};

const operations = [
    adv,
    bxl,
    bst,
    jnz,
    bxc,
    out,
    bdv,
    cdv,
];

logIfVerboseEnv('registry:\n', registry);
logIfVerboseEnv('memory:\n', memory);

const run = () => {
    const val = 0b100000000000000000000000000000000000000000000000;
    for (let i = 0; i < 8; i++) {
        outPut = [];
        registry = [0, 0, 0]
        registry[0] = val + i;
        pointer = 0;
        console.log('---');
        console.log(i, registry);
        while (pointer < memory.length) {
            const op = readLitteral(pointer);
            logIfVerboseEnv('pointer:', pointer, 'op:', op);
            operations[op](pointer);
            incPointer();
        }
        console.log(outPut.length, ': ', outPut.join(','));
    }

};

const getOutputForValue = (val) => {
    outPut = [];
    registry = [0, 0, 0]
    registry[0] = val;
    pointer = 0;
    console.log('---');
    console.log(val, registry);
    while (pointer < memory.length) {
        const op = readLitteral(pointer);
        logIfVerboseEnv('pointer:', pointer, 'op:', op);
        operations[op](pointer);
        incPointer();
    }
    return outPut;
}

// do a DFS to find the value where the output matches the memory by adding 3 bits at the time to the
// value and testing that the current output matches the end of the memory array.
const findValue = (val) => {
    let values = [];
    for (let i = 0; i < 8; i++) {
        const newVal = (val * 8) + i;
        const output = getOutputForValue(newVal);
        const matched = output.join(',') === memory.slice(-output.length).join(',');
        const correctLength = output.length === memory.length;
        if (matched && correctLength) {
            console.log('found:', newVal);
            process.exit(0);
        } else if (matched && output.length < memory.length) {
            const res = findValue(newVal);
            values = values.concat(res);
        }
    }
    return values;
}

const run2 = () => {
        //0b011000011011000000000,
    const vals = [
        0b011000011011000000000000,
        0b011000011011000000000001,
        0b011000011011000000000010,
        0b011000011011000000000011,
        0b011000011011000000000100,
        0b011000011011000000000101,
        0b011000011011000000000110,
        0b011000011011000000000111,
    ];
    vals.forEach((val) => {
        outPut = [];
        registry = [0, 0, 0]
        registry[0] = val;
        pointer = 0;
        console.log('---');
        console.log(val, registry);
        while (pointer < memory.length) {
            const op = readLitteral(pointer);
            logIfVerboseEnv('pointer:', pointer, 'op:', op);
            operations[op](pointer);
            incPointer();
        }
        console.log(outPut.length, ': ', outPut.join(','));
    });
};

const part2 = findValue(0);

console.log('part2:', part2.sort());

//run2();
