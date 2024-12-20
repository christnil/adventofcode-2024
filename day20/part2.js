const { log } = require('console');
const fs = require('fs');

const raw = fs.readFileSync('./input.txt', 'utf8')
const rows = raw.split('\n')


const logIfVerboseEnv = (...args) => {
  if (process.env.VERBOSE) {
    console.log(...args);
    }
};

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

let source = [0, 0];
let sink = [70, 70];
const map = rows.reduce((acc, row, x) => {
    acc[x] = acc[x] || {};
    row.split('').forEach((cell, y) => {
        if (cell === 'S') {
            source = [x, y];
        } else if (cell === 'E') {
            sink = [x, y];
        }
        acc[x][y] = cell;
    });
    return acc;
}, {});

const IS_TEST = rows.length < 20;

const inMap = (x, y) => {
    return x >= 0 && y >= 0 && x < rows[0].length && y < rows.length;
}

const canVisit = (x, y) => {
    if (!inMap(x, y)) return false;
    if (map[x][y] === '#') return false;
    return true;
};

const visited = {};
const parent = {};
const tryVisit = (x, y, at, from) => {
    if (!canVisit(x, y, at)) return;
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

const bfs = () => {
    const compare = ([_, t1], [__, t2]) => {
        return t1 - t2;
    };
    const queue = priorityQueue(compare);
    queue.enqueue([sink, 0, undefined]);
    while (!queue.isEmpty()) {
        const [[x, y], at, parent] = queue.dequeue();
        if (tryVisit(x, y, at, parent) && (x !== source[0] || y !== source[1])) {
            const next = at + 1;
            queue.enqueue([[x - 1, y], next, [x, y]]);
            queue.enqueue([[x + 1, y], next, [x, y]]);
            queue.enqueue([[x, y - 1], next, [x, y]]);
            queue.enqueue([[x, y + 1], next, [x, y]]);
        }
    }
};

const manhattanDistance = ([x1, y1], [x2, y2]) => {
    return Math.abs(x1 - x2) + Math.abs(y1 - y2);
}

const CHEET_TIME = 20
const findCheetsForPosition = (x, y) => {
    if (map[x][y] === 'X') {
        return [];
    }
    const current = visited[x][y];
    const possibleToReach = [];
    for (let i = x - CHEET_TIME; i <= x + CHEET_TIME; i++) {
        for (let j = y - CHEET_TIME; j <= y + CHEET_TIME; j++) {
            if (canVisit(i, j) && manhattanDistance([x, y], [i, j]) <= CHEET_TIME) {
                possibleToReach.push([i, j]);
            }
        }
    }
    return possibleToReach.map(([px, py]) => {
        const reachable = visited[px][py];
        const gain = current - manhattanDistance([x, y], [px, py]) - reachable;
        if (gain === 64) {
            logIfVerboseEnv('Cheet at', [x, y], 'to', [px, py], 'current', current, 'reachable', reachable);
        }
        return gain;
    }).filter(x => x > 0);
}

const findAllCheets = () => {
    const cheets = [];
    for (let x = 0; x < rows[0].length; x++) {
        for (let y = 0; y < rows.length; y++) {
            if (canVisit(x, y)) {
                cheets.push(...findCheetsForPosition(x, y));
            }
        }
    }
    return cheets;
}



const draw = (path) => {
    const grid = Array.from({ length: sink[0] + 1 }, () => Array.from({ length: sink[1] + 1 }, () => '.'));
    path.forEach(([x, y]) => grid[y][x] = 'O');
    coords.slice(0, LIMIT).forEach(([x, y]) => grid[y][x] = '#');
    console.log(grid.map(row => row.join('')).join('\n'));
}

bfs();
const cheets = findAllCheets();
const countByValue = cheets.reduce((acc, x) => {
    acc[x] = acc[x] || 0;
    acc[x]++;
    return acc;
}, {});
const cheetArray = Object.entries(countByValue)
    .map(([k, v]) => [parseInt(k), v])
    .sort((a, b) => a[0] - b[0])
    .filter(([k, v]) => k >= (IS_TEST ? 50 : 100));
logIfVerboseEnv('Cheets:', JSON.stringify(cheetArray, null, 2));

const countCheetsOver100 = cheetArray.reduce((acc, [_, v]) => acc + v, 0);

console.log('Part2: ', countCheetsOver100);
