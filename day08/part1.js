const fs = require('fs');

const raw = fs.readFileSync('./input.txt', 'utf8')

const logIfVerboseEnv = (...args) => {
  if (process.env.VERBOSE) {
    console.log(...args);
    }
};

logIfVerboseEnv('raw:', raw);

const rows = raw.split('\n');

/* { 'A': [ [1, 1], [2, 3]] } */
const antennaCoordsPerType = {};
const minX = 0;
const minY = 0;
const maxX = rows[0].length - 1;
const maxY = rows.length - 1;
rows.forEach((row, y) => {
  const cols = row.split('');
  cols.forEach((type, x) => {
    if (type !== '.') {
        if (!antennaCoordsPerType[type]) {
            antennaCoordsPerType[type] = [];
        }
        antennaCoordsPerType[type].push([x, y]);
    }
  });
});

const subtract = (a, b) => a.map((v, i) => v - b[i]);
const add = (a, b) => a.map((v, i) => v + b[i]);
const insideMap = (coords) => coords[0] >= minX && coords[0] <= maxX && coords[1] >= minY && coords[1] <= maxY;
const visited = {};

Object.keys(antennaCoordsPerType).forEach((type) => {
    const antennaCoords = antennaCoordsPerType[type];
    for (let i = 0; i < antennaCoords.length; i++) {
        const a1 = antennaCoords[i];
        for (let j = i + 1; j < antennaCoords.length; j++) {
            if (i === j) {
                continue;
            }
            const a2 = antennaCoords[j];
            const diff = subtract(a1, a2);
            const pos1 = add(a1, diff);
            const pos2 = subtract(a2, diff);
            if (insideMap(pos1)) {
                visited[JSON.stringify(pos1)] = true;
            }
            if (insideMap(pos2)) {
                visited[JSON.stringify(pos2)] = true;
            }
        }
    }
});

console.log('Part1: ', Object.keys(visited).length);
