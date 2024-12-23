const fs = require('fs');

const raw = fs.readFileSync('./input.txt', 'utf8')
const rows = raw.split('\n')


const logIfVerboseEnv = (...args) => {
  if (process.env.VERBOSE) {
    console.log(...args);
    }
};

const numKeyPadPositions = {
    7: [0, 0],
    8: [1, 0],
    9: [2, 0],
    4: [0, 1],
    5: [1, 1],
    6: [2, 1],
    1: [0, 2],
    2: [1, 2],
    3: [2, 2],
    0: [1, 3],
    A: [2, 3],
}

const arrowKeyPadPositions = {
    '^': [1, 0],
    'A': [2, 0],
    '<': [0, 1],
    'v': [1, 1],
    '>': [2, 1],
}

const getPathBetweenNums = (start, end) => {
    const [x1, y1] = numKeyPadPositions[start];
    const [x2, y2] = numKeyPadPositions[end];
    const xDiff = x2 - x1;
    const yDiff = y2 - y1;
    const xDir = xDiff < 0 ? '<' : '>';
    const yDir = yDiff < 0 ? '^' : 'v';
    const paths = new Set();
    if (y1 !== 3 || x2 !== 0) {
        paths.add(xDir.repeat(Math.abs(xDiff)) + yDir.repeat(Math.abs(yDiff)));
    }
    if (x1 !== 0 || y2 !== 3) {
        paths.add(yDir.repeat(Math.abs(yDiff)) + xDir.repeat(Math.abs(xDiff)));
    }
    return [...paths];
}

const getPathBetweenArrows = (start, end) => {
    const [x1, y1] = arrowKeyPadPositions[start];
    const [x2, y2] = arrowKeyPadPositions[end];
    const xDiff = x2 - x1;
    const yDiff = y2 - y1;
    const xDir = xDiff < 0 ? '<' : '>';
    const yDir = yDiff < 0 ? '^' : 'v';
    const paths = new Set();
    if (y1 !== 0 || x2 !== 0) {
        paths.add(xDir.repeat(Math.abs(xDiff)) + yDir.repeat(Math.abs(yDiff)));
    }
    if (x1 !== 0 || y2 !== 0) {
        paths.add(yDir.repeat(Math.abs(yDiff)) + xDir.repeat(Math.abs(xDiff)));
    }
    return [...paths];
}


/**
 * Position is { value: string, coord: [number, number] }
 */

const getPathBetween = (start, end) => {
    const selector = start === 'A' ? end : start;
    if (numKeyPadPositions[selector]) {
        return getPathBetweenNums(start, end);
    } else {
        return getPathBetweenArrows(start, end);
    }
}

const cache = {};

/**
* Finds the minimum number of steps required to reach the code with a given number of robots
* in between
* @param {string} code String containing nubmers and the letter A 
* @returns Number
*/
const findShortestInputForDirectionsRec = (code, depth) => {
    if (cache[code + depth]) {
        return cache[code + depth];
    }
    if (depth === 0) {
        return code.length;
    }
    let paths = code.split('').reduce((acc, curr, i, arr) => {
        if (i === 0) {
            return [...acc, getPathBetween('A', curr)];
        }
        return [...acc, getPathBetween(arr[i - 1], curr)];
    }, []);
    const minInputLength = paths.reduce((acc, curr) => {
        const options = curr.map(path => findShortestInputForDirectionsRec(path + 'A', depth - 1));
        return acc + Math.min(...options);
    }, 0);
    cache[code + depth] = minInputLength;
    return minInputLength;
}

/**
 * Finds the minimum number of steps required to reach the code with a given number of robots
 * in between
 * @param {string} code String containing nubmers and the letter A 
 * @returns Number
 */
const findShortestInputFor = (code, numberOfDirRobots = 2) => {
    let paths = code.split('').reduce((acc, curr, i, arr) => {
        if (i === 0) {
        return [...acc, getPathBetween('A', curr)];
        }
        return [...acc, getPathBetween(arr[i - 1], curr)];
    }, []);
    const minInputLength = paths.reduce((acc, curr) => {
        const options = curr.map(path => findShortestInputForDirectionsRec(path + 'A', numberOfDirRobots));
        return acc + Math.min(...options);
    }, 0);
    return minInputLength;
}

const part1 = rows.reduce((acc, row) => {
    const shortestInput = findShortestInputFor(row, 2);
    const rowNumValue = parseInt(row, 10);
    const accValue = shortestInput * rowNumValue;
    logIfVerboseEnv(`Row: ${row}, Shortest Input: ${shortestInput}, Row Num: ${rowNumValue}, Acc Value: ${accValue}`);
    return acc + accValue;
}, 0);

const part2 = rows.reduce((acc, row) => {
    const shortestInput = findShortestInputFor(row, 25);
    const rowNumValue = parseInt(row, 10);
    const accValue = shortestInput * rowNumValue;
    logIfVerboseEnv(`Row: ${row}, Shortest Input: ${shortestInput}, Row Num: ${rowNumValue}, Acc Value: ${accValue}`);
    return acc + accValue;
}, 0);

console.log('Part 1: ', part1)
console.log('Part 2: ', part2)
