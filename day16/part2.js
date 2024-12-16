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

const index = {};
let robotPosition = undefined;
const boxes = [];
const wallDictionary = {};
const rows = map.split('\n');
rows.forEach((row, x) => {
    row.split('').forEach((cell, y) => {
        if (cell === BOX) {
            index[toKey([x, 2 * y])] = boxes.length;
            index[toKey([x, 2 * y +1])] = boxes.length;
            boxes.push([x, 2 * y]);
        }
        if (cell === WALL) {
            wallDictionary[toKey([x, 2 * y])] = WALL;
            wallDictionary[toKey([x, 2 * y + 1])] = WALL;
        }
        if (cell === ROBOT) {
            robotPosition = [x, 2 * y];
        }
    });
});

const isWall = (key) => wallDictionary[key] === WALL;

const drawMap = () => {
    let map = '';
    for (let x = 0; x < rows.length; x++) {
        for (let y = 0; y < 2 * rows[x].length; y++) {
            const key = toKey([x, y]);
            if (key in wallDictionary) {
                map += WALL;
            } else if (x === robotPosition[0] && y === robotPosition[1]) {
                map += ROBOT;
            } else if (key in index) {
                const box = boxes[index[key]];
                map += box[1] === y ? '[' : ']';
            } else {
                map += EMPTY;
            }
        }
        map += '\n';
    }
    return map;
}

const isEmpty = (key) => {
    return !isWall(key) && !(key in index);
}
const isMoveable = (key) => {
    return isEmpty(key) || index[key] >= 0;
}

const canMoveBox = (id, direction) => {
    const box = boxes[id];
    const newPosition = box.map((coord, i) => coord + directions[direction][i]);
    const newKey1 = toKey(newPosition);
    const newKey2 = toKey([newPosition[0], newPosition[1] + 1]);
    if (isWall(newKey1) || isWall(newKey2)) {
        return false;
    }
    const isLeft = direction === '<';
    const isRight = direction === '>';
    if (isLeft) {
        if (isEmpty(newKey1)) {
            return true;
        }
        if (isMoveable(newKey1)) {
            return canMoveBox(index[newKey1], direction);
        }
    }
    if (isRight) {
        if (isEmpty(newKey2)) {
            return true;
        }
        if (isMoveable(newKey2)) {
            return canMoveBox(index[newKey2], direction);
        }
    }

    const leftHalf = isEmpty(newKey1) || (isMoveable(newKey1) && canMoveBox(index[newKey1], direction));
    const rightHalf = isEmpty(newKey2) || (isMoveable(newKey2) && canMoveBox(index[newKey2], direction));
    return leftHalf && rightHalf;
}

const moveBox = (id, direction) => {
    const box = boxes[id];
    const newPosition = box.map((coord, i) => coord + directions[direction][i]);
    const oldKey1 = toKey(box);
    const oldKey2 = toKey([box[0], box[1] + 1]);
    const newKey1 = toKey(newPosition);
    const newKey2 = toKey([newPosition[0], newPosition[1] + 1]);
    const isLeft = direction === '<';
    const isRight = direction === '>';
    if (isLeft) {
        if (!isEmpty(newKey1)) {
            moveBox(index[newKey1], direction);
        }
    } else if (isRight) {
        if (!isEmpty(newKey2)) {
            moveBox(index[newKey2], direction);
        }
    } else if (index[newKey1] === index[newKey2] && index[newKey1] !== undefined) {
        moveBox(index[newKey1], direction);
    } else {
        if (!isEmpty(newKey1)) {
            moveBox(index[newKey1], direction);
        }
        if (!isEmpty(newKey2)) {
            moveBox(index[newKey2], direction);
        }
    }
    delete index[oldKey1];
    delete index[oldKey2];
    index[newKey1] = id;
    index[newKey2] = id;
    boxes[id] = newPosition;
};

const tryMoveRobot = (direction) => {
    const moveArray = directions[direction];
    if (!moveArray) {
        return false;
    }
    const newPosition = robotPosition.map((pos, i) => pos + directions[direction][i]);
    const newKey = toKey(newPosition);
    if (isEmpty(newKey)) {
        robotPosition = newPosition;
        return true;
    } else if (isWall(newKey)) {
        return false;
    } else if (canMoveBox(index[newKey], direction)) {
        moveBox(index[newKey], direction);
        robotPosition = newPosition;
        return true;
    }
    return false;
}

const countScores = () => {
    let score = 0;
    boxes.forEach(box => {
        score += box[0] * 100 + box[1];
    });
    return score;
};

console.log(drawMap());
moves.split('').forEach(move => {
    if (directions[move]) {
        tryMoveRobot(move);
        logIfVerboseEnv(move);
        logIfVerboseEnv(drawMap());
    }
});
console.log(drawMap());

let part2 = countScores();
console.log('Part2: ', part2);
