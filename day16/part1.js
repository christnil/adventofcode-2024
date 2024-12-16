const fs = require('fs');
const { get } = require('http');

const raw = fs.readFileSync('./input.txt', 'utf8')

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

//logIfVerboseEnv('raw:\n', raw);

const directions = {
    '^': [-1, 0],
    'v': [1, 0],
    '<': [0, -1],
    '>': [0, 1],
}

const WALL = '#';
const EMPTY = '.';
const START = 'S';
const SINK = 'E';


const toKey = (pos) => pos.join(',');
const toDirectionalKey = (pos, direction) => toKey([...pos, ...direction]);
const rotateCounterClockwise = (direction) => [-direction[1], direction[0]];

const walls = {};
let sourcePosition = undefined;
let sinkPosition = undefined;
const sourceDirection = [0, 1];
const rows = raw.split('\n');
rows.forEach((row, x) => {
    row.split('').forEach((cell, y) => {
        switch (cell) {
            case WALL:
                walls[toKey([x, y])] = true;
                break;
            case START:
                sourcePosition = [x, y];
                break;
            case SINK:
                sinkPosition = [x, y];
                break;
            default:
                break;
        }
    });
});

const parents = {};
const djikstra = () => {
    let best = undefined;
    const queue = priorityQueue((a, b) => a[0] - b[0]);
    queue.enqueue([0, sourcePosition, sourceDirection, null]);
    while (!queue.isEmpty() && (!best || queue.peek()[0] <= best)) {
        const [distance, position, direction, parent] = queue.dequeue();
        const key = toKey(position);
        if (parents[key]) {
            if (parents[key].cost === distance && parent) {
                parents[key].parent.push(parent[0]);
            }
            continue;
        }
        parents[key] = {
            cost: distance,
            parent: parent && [parent[0]],
        };
        if (position[0] === sinkPosition[0] && position[1] === sinkPosition[1]) {
            return best = distance;
        }
        const nextParent = [position, direction];
        let nextPosition = [position[0] + direction[0], position[1] + direction[1]];
        let nexDirection = direction;
        if (!walls[toKey(nextPosition)]) {
            queue.enqueue([distance + 1, nextPosition, nexDirection, nextParent]);
        }
        for (let i = 1; i <= 3; i++) {
            nexDirection = rotateCounterClockwise(nexDirection);
            nextPosition = [position[0] + nexDirection[0], position[1] + nexDirection[1]];
            if (!walls[toKey(nextPosition)]) {
                queue.enqueue([distance + 1 + Math.min(i * 1000, (4 - i) * 1000), nextPosition, nexDirection, nextParent]);
            }
        }
    }
    return best;
};

const getPath = () => {
    const path = [];
    let key = toKey(sinkPosition);
    while (key) {
        const { cost, parent } = (parents[key] || {});
        if (!parent) {
            return path;
        }
        path.unshift({ position: parent, cost });
        key = toKey(parent);
    }
    return path;
}

const distance = djikstra();
const part1 = () => {
    const path = getPath();
    logIfVerboseEnv(path);
    console.log('Part1: ', distance);
};

const getPathTiles = (pos) => {
    let key = toKey(pos);
    const { parent } = (parents[key] || {});
    const tiles = [pos];
    (parent || []).forEach(p => {
        tiles.push(...getPathTiles(p));
    });
    return tiles;
}

const drawMap = (visitedSet) => {
    const map = [];
    for (let x = 0; x < rows.length; x++) {
        const row = [];
        for (let y = 0; y < rows[x].length; y++) {
            const key = toKey([x, y]);
            if (key === toKey(sourcePosition)) {
                row.push('S');
            } else if (key === toKey(sinkPosition)) {
                row.push('E');
            } else if (visitedSet.has(key)) {
                row.push('O');
            } else if (walls[key]) {
                row.push(WALL);
            } else {
                row.push(EMPTY);
            }
        }
        map.push(row.join(''));
    }
    console.log(map.join('\n'));
}

const part2 = () => {
    const path = getPath();
    const pathTiles = getPathTiles(sinkPosition);
    const asKeys = pathTiles.map(p => toKey(p));
    const uniqueKeys = new Set(asKeys);
    drawMap(uniqueKeys);
    console.log('Part2: ', uniqueKeys.size);
}

part1();
part2();
