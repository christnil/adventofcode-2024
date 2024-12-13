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

let visited = new Set();
const getRegion = (source) => {
    if (visited.has(JSON.stringify(source))) {
        return { val: getPosition(source.x, source.y), size: 0, fences: 0};
    }
    const queue = [source];
    visited.add(JSON.stringify(source));
    let size = 1;
    let fenceLength = 0;
    let fences = [];
    while (queue.length > 0) {
        const { x, y } = queue.shift();
        getEdges({x, y}).forEach((next) => {
            if (getPosition(next.x, next.y) !== getPosition(x, y)) {
                fences.push({x: (x + next.x)/2, y: (y + next.y)/2, dir: next.x > x || next.y > y ? '+' : '-'});
                fenceLength++;
            } else if (!visited.has(JSON.stringify(next))) {
                visited.add(JSON.stringify(next));
                size++;
                queue.push(next);
            }
        });
    }
    return { val: getPosition(source.x, source.y), size, fences };
};

let part1 = 0;
matr.forEach((row, x) => {
    row.forEach((_, y) => {
        const result = getRegion({x, y});
        if (result.size > 0) {
            logIfVerboseEnv('result:', result);
            part1 += result.size * result.fences.length;
        }
    });
});

const groupFences = (fences) => {
    const verticals = fences.filter(f => f.x % 1 === 0);
    logIfVerboseEnv('verticals:', verticals);
    const sortedVerticals = verticals.sort((a, b) => {
        if (a.y === b.y) {
            return a.x - b.x;
        }
        return a.y - b.y;
    });
    logIfVerboseEnv('sortedVerticals:', sortedVerticals);
    const numberOfVerticalGroups = sortedVerticals.reduce((acc, next) => {
        if (next.y !== acc.last.y || Math.abs(next.x - acc.last.x) > 1 || next.dir !== acc.last.dir) {
            acc.count++;
        }
        acc.last = next;
        return acc;
    }, { count: 0, last: {} });
    const horizontals = fences.filter(f => f.y % 1 === 0);
    logIfVerboseEnv('horizontals:', horizontals);
    const sortedHorizontals = horizontals.sort((a, b) => {
        if (a.x === b.x) {
            return a.y - b.y;
        }
        return a.x - b.x;
    });
    logIfVerboseEnv('sortedHorizontals:', sortedHorizontals);
    const numberOfHorizontalGroups = sortedHorizontals.reduce((acc, next) => {
        if (next.x !== acc.last.x || Math.abs(next.y - acc.last.y) > 1 || next.dir !== acc.last.dir) {
            acc.count++;
        }
        acc.last = next;
        return acc;
    }, { count: 0, last: {} });
    return numberOfVerticalGroups.count + numberOfHorizontalGroups.count;
}


console.log('Part1: ', part1);

visited = new Set();
let part2 = 0;
matr.forEach((row, x) => {
    row.forEach((_, y) => {
        const result = getRegion({x, y});
        if (result.size > 0) {
            const gFences = groupFences(result.fences);
            logIfVerboseEnv('result:', { ...result, gFences });
            part2 += result.size * gFences;
        }
    });
});
console.log('Part2: ', part2);
