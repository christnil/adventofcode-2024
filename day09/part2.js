const fs = require('fs');
const { parse } = require('path');

const raw = fs.readFileSync('./input.txt', 'utf8')

const logIfVerboseEnv = (...args) => {
  if (process.env.VERBOSE) {
    console.log(...args);
    }
};

logIfVerboseEnv('raw:', raw);

const files = raw.split('').map(Number);

const disk = {}; /* { 0: { size 2, value: 3 }} */
let addr = 0;
let pAddr = undefined;
let fileId = 0;
let free = false;
const freeOfSize = {};
const fileList = [];

const addFreeAddressOfSize = (size, addr) => {
    if (!freeOfSize[size]) {
        freeOfSize[size] = [];
    }
    freeOfSize[size].push(addr);
    freeOfSize[size].sort((a, b) => a - b);
}

const removeFreeAddressOfSize = (size, addr) => {
    freeOfSize[size] = freeOfSize[size].filter(x => x !== addr);
    if (freeOfSize[size].length === 0) {
        delete freeOfSize[size];
    }
}

const getFirstFreeAddressOfSize = (size) => {
    return Object.keys(freeOfSize)
    .map((key) => ({ size: parseInt(key), address: freeOfSize[key][0] }))
    .filter(x => x.size >= size)
    .sort((a, b) => a.address - b.address)
    .find(x => x);
}

for (let i = 0; i < files.length; i++) {
    const size = files[i];
    const value = free ? 0 : fileId++;
    disk[addr] = { size, value, free, prevAddr: pAddr };
    if (free) {
        addFreeAddressOfSize(size, addr);
    } else {
        fileList.push(addr);
    }
    free = !free;
    pAddr = size != 0 ? addr : pAddr;
    addr += size;
}

logIfVerboseEnv('disk:', JSON.stringify(disk, null, 2));

const getDiskString = () => {
    let addr = 0;
    let str = '';
    while (disk[addr]) {
        const { size, value, free } = disk[addr];
        if (free) {
            str += '.'.repeat(size);
        } else {
            str += value.toString().repeat(size);
        }
        addr += size;
    }
    return str;
}

const checkSum = () => {
    let addr = 0;
    let sum = 0;
    while (disk[addr]) {
        const { size, value, free } = disk[addr];
        if (!free) {
            for (let i = addr; i < addr + size; i++) {
                sum += i * value;
            }
        }
        addr += size;
    }
    return sum;
}

fileList.reverse().forEach((addr) => {
    logIfVerboseEnv('diskStr:', getDiskString());
    const { size, value, prevAddr } = disk[addr];
    if (size === 0) {
        return;
    }
    const option = getFirstFreeAddressOfSize(size);
    if (option && option.address < addr) {
        const { size: freeSize, address: freeAddr } = option;
        freeOfSize[freeSize].shift();
        disk[freeAddr].size = size;
        disk[freeAddr].value = value;
        disk[freeAddr].free = false;
        if (freeSize > size) {
            addFreeAddressOfSize(freeSize - size, freeAddr + size);
            disk[freeAddr + size] = { size: freeSize - size, value: 0, free: true, prevAddr: freeAddr };
        }
        disk[addr].free = true;
        addFreeAddressOfSize(size, addr);
        disk[addr].value = 0;
        //logIfVerboseEnv('first:', getDiskString());
        // join with prev and next if they are free
        if (disk[addr + size] && disk[addr + size].free) {
            removeFreeAddressOfSize(disk[addr + size].size, addr + size);
            removeFreeAddressOfSize(disk[addr].size, addr);
            disk[addr].size += disk[addr + size].size;
            addFreeAddressOfSize(disk[addr].size, addr);
            delete disk[addr + size];
        }
        //logIfVerboseEnv('merge next:', getDiskString());
        if (disk[addr].prevAddr && disk[disk[addr].prevAddr] && disk[disk[addr].prevAddr].free) {
            const pAddre = disk[addr].prevAddr;
            removeFreeAddressOfSize(disk[pAddre].size, pAddre);
            removeFreeAddressOfSize(disk[addr].size, addr);
            disk[pAddre].size += disk[addr].size;
            addFreeAddressOfSize(disk[pAddre].size, pAddre);
            delete disk[addr];
        }
        //logIfVerboseEnv('merge prev:', getDiskString());
    }
});

//logIfVerboseEnv('disk:', JSON.stringify(disk, null, 2));
logIfVerboseEnv('diskStr:', getDiskString());

console.log('Part2: ', checkSum());
