const fs = require('fs');

const raw = fs.readFileSync('./input.txt', 'utf8')

const logIfVerboseEnv = (...args) => {
  if (process.env.VERBOSE) {
    console.log(...args);
    }
};

logIfVerboseEnv('raw:', raw);

const matr = raw.split('\n').map(r => r.split('').map(Number));
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
  row.forEach((val, y) => {
    if (val === 0) {
      sources.push({ x, y });
    }
    if (val === 9) {
      sinks.push({ x, y });
    }
    if (val + 1 === getPosition(x, y + 1)) {
        addEdge({x,y}, { x, y: y + 1 });
    }
    if (val + 1 === getPosition(x, y - 1)) {
        addEdge({x,y}, { x, y: y - 1 });
    }
    if (val + 1 === getPosition(x + 1, y)) {
        addEdge({x,y}, { x: x+1, y });
    }
    if (val + 1 === getPosition(x - 1, y)) {
        addEdge({x,y}, { x :x-1, y });
    }
  });
});


logIfVerboseEnv('edges:', edges);
logIfVerboseEnv('sources:', sources);
logIfVerboseEnv('sinks:', sinks);

const sinkSet = new Set(sinks.map(s => JSON.stringify(s)));
const isSink = (x, y) => sinkSet.has(JSON.stringify({ x, y }));

const getReachableSinkCount = (source) => {
    const queue = [source];
    const visited = new Set([JSON.stringify(source)]);
    let count = 0;
    while (queue.length > 0) {
        const { x, y } = queue.shift();
        if (isSink(x, y)) {
            count++;
        }
        getEdges({x, y}).forEach((next) => {
            if (!visited.has(JSON.stringify(next))) {
                visited.add(JSON.stringify(next));
                queue.push(next);
            }
        });
    }
    return count;
};

const getDistinctRoutesCount = (source) => {
    const queue = [source];
    let count = 0;
    while (queue.length > 0) {
        const { x, y } = queue.shift();
        if (isSink(x, y)) {
            count++;
        }
        getEdges({x, y}).forEach((next) => {
            queue.push(next);
        });
    }
    return count;
};

const part1 = sources.map(getReachableSinkCount).reduce((a, b) => a + b, 0);
console.log('Part1: ', part1);

const part2 = sources.map(getDistinctRoutesCount).reduce((a, b) => a + b, 0);
console.log('Part2: ', part2);
