const { log } = require('console');
const fs = require('fs');

const raw = fs.readFileSync('./input.txt', 'utf8')

const logIfVerboseEnv = (...args) => {
  if (process.env.VERBOSE) {
    console.log(...args);
    }
};

logIfVerboseEnv('raw:\n', raw);

const [map, moves] = raw.split('\n\n')
logIfVerboseEnv('map:\n', map);
logIfVerboseEnv('moves:\n', moves);

const directions = {
    '^': [-1, 0],
    'v': [1, 0],
    '<': [0, -1],
    '>': [0, 1],
}

const WALL = '#';
const EMPTY = '.';
const ROBOT = '@';
const BOX = 'O';

const toKey = (pos) => pos.join(',');

const positions = {};
let robotPosition = undefined;
const rows = map.split('\n');
rows.forEach((row, x) => {
    row.split('').forEach((cell, y) => {
        if (cell === WALL || cell === BOX) {
            positions[toKey([x, y])] = cell;
        }
        if (cell === ROBOT) {
            robotPosition = [x, y];
        }
    });
});

const drawMap = () => {
    let map = '';
    for (let x = 0; x < rows.length; x++) {
        for (let y = 0; y < rows[x].length; y++) {
            const key = toKey([x, y]);
            if (key in positions) {
                map += positions[key];
            } else if (x === robotPosition[0] && y === robotPosition[1]) {
                map += ROBOT;
            } else {
                map += EMPTY;
            }
        }
        map += '\n';
    }
    return map;
}

const isEmpty = (key) => {
    return !(key in positions);
}
const isMoveable = (key) => {
    return isEmpty(key) || positions[key] === BOX;
}

const tryMoveBox = (position, direction) => {
    const key = toKey(position);
    if (positions[key] !== BOX) {
        return false;
    }
    const newPosition = position.map((pos, i) => pos + directions[direction][i]);
    const newKey = toKey(newPosition);
    if (isEmpty(newPosition)) {
        positions[newKey] = BOX;
        delete positions[key];
        return true;
    } if (isMoveable(newKey)) {
        const moved = tryMoveBox(newPosition, direction);
        if (moved) {
            positions[newKey] = BOX;
            delete positions[key];
            return true;
        }
    }
    return false;
}

const tryMoveRobot = (direction) => {
    const moveArray = directions[direction];
    if (!moveArray) {
        return false;
    }
    const newPosition = robotPosition.map((pos, i) => pos + directions[direction][i]);
    const newKey = toKey(newPosition);
    if (isEmpty(newPosition)) {
        robotPosition = newPosition;
        return true;
    } if (isMoveable(newKey)) {
        const moved = tryMoveBox(newPosition, direction);
        if (moved) {
            robotPosition = newPosition;
            return true;
        }
    }
    return false;
}

const countScores = () => {
    let score = 0;
    for (let x = 0; x < rows.length; x++) {
        for (let y = 0; y < rows[x].length; y++) {
            const key = toKey([x, y]);
            if (positions[key] === BOX) {
                score += (100 * x) + y;
            }
        }
    }
    return score;
};

moves.split('').forEach(move => {
    if (directions[move]) {
        tryMoveRobot(move);
        logIfVerboseEnv(drawMap());
    }
});
console.log(drawMap());

let part1 = countScores();
console.log('Part1: ', part1);
