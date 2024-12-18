const fs = require('fs');

const raw = fs.readFileSync('./input.txt', 'utf8')

const logIfVerboseEnv = (...args) => {
  if (process.env.VERBOSE) {
    console.log(...args);
    }
};

logIfVerboseEnv('raw:\n', raw);

const [registryInput, programInput] = raw.split('\n\n')

const getAllNumbers = (str) => str.match(/\d+/g).map(Number);

const registry = getAllNumbers(registryInput);
const memory = getAllNumbers(programInput);
let pointer = 0;

const readLitteral = (addr) => memory[addr];
const readRegistry = (addr) => registry[addr];
const readCombo = (addr) => readLitteral(addr) < 4 ? readLitteral(addr) : readRegistry(readLitteral(addr) - 4);
const writeMemory = (addr, value) => memory[addr] = (value & 0b111);
const writeRegistry = (addr, value) => registry[addr] = value;
const writePointer = (value) => pointer = value;
const incPointer = () => pointer += 2;

const outPut = [];
const writeOutput = (value) => outPut.push(value);

const xor = (a, b) => Number(BigInt(a) ^ BigInt(b));

const adv = (addr) => {
    const n = readRegistry(0);
    const d = 1 << readCombo(addr + 1);
    const res = Math.floor(n / d);
    writeRegistry(0, res);
    logIfVerboseEnv('adv');
    logIfVerboseEnv('n: ', n, ', d: ', d, ', res:', res);
};

const bxl = (addr) => {
    const a = readRegistry(1);
    const b = readLitteral(addr + 1);
    const res = xor(a, b);
    writeRegistry(1, res);
    logIfVerboseEnv('bxl');
    logIfVerboseEnv('a: ', a, ', b: ', b, ', res:', res);
};

const bst = (addr) => {
    const a = readCombo(addr + 1);
    const res = a & 0b111;
    writeRegistry(1, res);
    logIfVerboseEnv('bst');
    logIfVerboseEnv('a: ', a, ', res:', res);
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
};

const out = (addr) => {
    const a = readCombo(addr + 1);
    const res = a & 0b111;
    writeOutput(res);
    logIfVerboseEnv('out');
    logIfVerboseEnv('a: ', a, ', res:', res);
};

const bdv = (addr) => {
    const n = readRegistry(0);
    const d = 1 << readCombo(addr + 1);
    const res = Math.floor(n / d);
    writeRegistry(1, res);
    logIfVerboseEnv('bdv');
    logIfVerboseEnv('n: ', n, ', d: ', d, ', res:', res);
};

const cdv = (addr) => {
    const n = readRegistry(0);
    const d = 1 << readCombo(addr + 1);
    const res = Math.floor(n / d);
    writeRegistry(2, res);
    logIfVerboseEnv('cdv');
    logIfVerboseEnv('n: ', n, ', d: ', d, ', res:', res);
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
    while (pointer < memory.length) {
        const op = readLitteral(pointer);
        logIfVerboseEnv('pointer:', pointer, 'op:', op);
        operations[op](pointer);
        incPointer();
    }
    console.log('Part1: ', outPut.join(','));
};

run();
