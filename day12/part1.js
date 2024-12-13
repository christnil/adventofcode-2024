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

const matr = raw.split('\n').map(r => r.split(''));

const getPosition = (x, y) => {
    if (x < 0 || y < 0) {
        return null;
    }
    if (x >= matr.length || y >= matr[0].length) {
        return null;
    }
    return matr[x][y];
};

const edges = {};
const addEdge = (a, b) => {
    const key = JSON.stringify(a);
    if (!edges[key]) {
        edges[key] = [];
    }
    edges[key].push(b);
}
const getEdges = (a) => edges[JSON.stringify(a)] || [];
const sources = [];
const sinks = [];
matr.forEach((row, x) => {
  row.forEach((_, y) => {
    addEdge({x,y}, { x, y: y + 1 });
    addEdge({x,y}, { x, y: y - 1 });
    addEdge({x,y}, { x: x + 1, y });
    addEdge({x,y}, { x: x - 1, y });
  });
});

const visited = new Set();
const getRegion = (source) => {
    if (visited.has(JSON.stringify(source))) {
        return { val: getPosition(source.x, source.y), size: 0, fenceLength: 0};
    }
    const queue = [source];
    visited.add(JSON.stringify(source));
    let size = 1;
    let fenceLength = 0;
    while (queue.length > 0) {
        const { x, y } = queue.shift();
        getEdges({x, y}).forEach((next) => {
            if (getPosition(next.x, next.y) !== getPosition(x, y)) {
                fenceLength++;
            } else if (!visited.has(JSON.stringify(next))) {
                visited.add(JSON.stringify(next));
                size++;
                queue.push(next);
            }
        });
    }
    return { val: getPosition(source.x, source.y), size, fenceLength };
};

let part1 = 0;
matr.forEach((row, x) => {
    row.forEach((_, y) => {
        const result = getRegion({x, y});
        if (result.size > 0) {
            logIfVerboseEnv('result:', result);
            part1 += result.size * result.fenceLength;
        }
    });
});


console.log('Part1: ', part1);

const part2 = 0;
console.log('Part2: ', part2);
