const fs = require('fs');
const path = require('path');

const raw = fs.readFileSync('./input.txt', 'utf8')
const rows = raw.split('\n')


const logIfVerboseEnv = (...args) => {
  if (process.env.VERBOSE) {
    console.log(...args);
    }
};

logIfVerboseEnv('raw:\n', raw);
logIfVerboseEnv('rows:\n', rows);

function priorityQueue(compare) {
    const queue = [];
    return {
        enqueue: (item) => {
            let i = queue.length - 1;
            while (i >= 0 && compare(item, queue[i]) < 0) { i--; }
            queue.splice(i + 1, 0, item);
        },
        dequeue: () => queue.shift(),
        peek: () => queue[0],
        isEmpty: () => queue.length === 0,
        size: () => queue.length,
    };
}

const IS_TEST = rows.length < 50;
const coords = rows.map(row => row.split(',').map(Number));

const broken = coords.reduce((acc, [x, y], i) => {
    acc[x] = acc[x] || {};
    acc[x][y] = i;
    return acc;
}, {});

const LIMIT = IS_TEST ? 12 : 1024;

const isBroken = (x, y, at) => broken[x] && broken[x][y] < at;

const source = [0, 0];
const sink = IS_TEST ? [6, 6] : [70, 70];

const canVisitAt = (x, y, at) => {
    if (x < 0 || y < 0) return false;
    if (x > sink[0] || y > sink[1]) return false;
    if (isBroken(x, y, at)) return false;
    return true;
};

let visited = {};
let parent = {};
const tryVisit = (x, y, at, from, timeLimit) => {
    if (!canVisitAt(x, y, timeLimit)) return;
    const current = visited[x] && visited[x][y];
    if (current > at || current === undefined) {
        visited[x] = visited[x] || {};
        visited[x][y] = at;
        parent[x] = parent[x] || {};
        parent[x][y] = from;
        return true;
    }
    return false;
};

const reset = () => {
    visited = {};
    parent = {};
};

const canBeBetter = ([[x, y], at], best) => {
    if (!best) return true;
    const d = Math.abs(x - sink[0]) + Math.abs(y - sink[1]);
    return d + at < best;
};

const aStar = (timeLimit) => {
    const compare = ([[x1, y1], t1], [[x2, y2], t2]) => {
        const d1 = Math.abs(x1 - sink[0]) + Math.abs(y1 - sink[1]);
        const d2 = Math.abs(x2 - sink[0]) + Math.abs(y2 - sink[1]);
        const e1 = d1 + t1;
        const e2 = d2 + t2;
        return e1 - e2;
    };
    const queue = priorityQueue(compare);
    queue.enqueue([source, 0, undefined]);
    let best = undefined;
    while (!queue.isEmpty() && canBeBetter(queue.peek(), best)) {
        const [[x, y], at, parent] = queue.dequeue();
        if (tryVisit(x, y, at, parent, timeLimit)) {
            if (x === sink[0] && y === sink[1]) {
                best = at;
            }
            const next = at + 1;
            queue.enqueue([[x - 1, y], next, [x, y]]);
            queue.enqueue([[x + 1, y], next, [x, y]]);
            queue.enqueue([[x, y - 1], next, [x, y]]);
            queue.enqueue([[x, y + 1], next, [x, y]]);
        }
    }
    const path = [sink];
    //let current = sink;
    //while (current && (current[0] !== source[0] || current[1] !== source[1])) {
    //    current = parent[current[0]][current[1]];
    //    path.unshift(current);
    //}
    return {
        path,
        best,
    }
};

const draw = (path) => {
    const grid = Array.from({ length: sink[0] + 1 }, () => Array.from({ length: sink[1] + 1 }, () => '.'));
    path.forEach(([x, y]) => grid[y][x] = 'O');
    coords.slice(0, LIMIT).forEach(([x, y]) => grid[y][x] = '#');
    console.log(grid.map(row => row.join('')).join('\n'));
}

let firstWorking = 0;
let firstBroken = coords.length;
while (firstWorking + 1 < firstBroken) {
    const mid = Math.floor((firstWorking + firstBroken) / 2);
    reset();
    const { best } = aStar(mid);
    if (best) {
        firstWorking = mid;
    } else {
        firstBroken = mid;
    }
}
console.log('Part2: ', firstBroken - 1, coords[firstBroken - 1]);

