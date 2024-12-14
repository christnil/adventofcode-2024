const { log } = require('console');
const fs = require('fs');
const { get } = require('http');

const raw = fs.readFileSync('./input.txt', 'utf8')
const rows = raw.split('\n');

const logIfVerboseEnv = (...args) => {
  if (process.env.VERBOSE) {
    console.log(...args);
    }
};

logIfVerboseEnv('raw:\n', raw);

const isTest = rows.length < 20;
const [xMax, yMax] = isTest ? [11, 7] : [101, 103];

const regex = /-?\d+\b/g;

const parseNumbersFromChunk = (chunk) => chunk.match(regex).map(Number);

const velocities = [];
const initPositions = [];
const equations = raw.split('\n').map(parseNumbersFromChunk).forEach(([x, y, vx, vy]) => {
    initPositions.push([x, y]);
    velocities.push([vx, vy]);
});
logIfVerboseEnv('positions:\n', initPositions);
logIfVerboseEnv('velocities:\n', velocities);

const move = (pos, vel, steps = 1) => {
    let x = (pos[0] + steps * vel[0]) % xMax;
    let y = (pos[1] + steps * vel[1]) % yMax;
    if (x < 0) {
        x += xMax;
    }
    if (y < 0) {
        y += yMax;
    }
    return [x, y];
};

const draw = (positions, counts = true) => {
    const grid = Array.from({ length: yMax }, () => Array(xMax).fill('.'));
    positions.forEach(([x, y]) => {
        if (!counts) {
            grid[y][x] = '#';
        } else {
            if (grid[y][x] === '.') {
                grid[y][x] = 0;
            }
            grid[y][x]++;
        }
    });
    return grid.map(row => row.join('')).join('\n');
};

const countsPerQaudrant = (positions) => {
    const counts = Array(4).fill(0);
    positions.forEach(([x, y]) => {
        if (x < Math.floor(xMax / 2) && y < Math.floor(yMax / 2)) {
            counts[0]++;
        } else if (x > Math.floor(xMax / 2) && y < Math.floor(yMax / 2)) {
            counts[1]++;
        } else if (x < Math.floor(xMax / 2) && y > Math.floor(yMax / 2)) {
            counts[2]++;
        } else if (x > Math.floor(xMax / 2) && y > Math.floor(yMax / 2)) {
            counts[3]++;
        }
    });
    logIfVerboseEnv('counts per quadrant:', counts);
    return counts;
}

const saftyFactor = (positions) => countsPerQaudrant(positions).reduce((acc, val) => acc * val, 1);

const drawFirstFiveSteps = () => {
    if (isTest && process.env.VERBOSE === 'true') {
        for (let i = 0; i < 6; i++) {
            const positions = initPositions.map((pos, j) => move(pos, velocities[j], i));
            console.log(`Step ${i}`);
            console.log(draw(positions));
        }
    }
}

drawFirstFiveSteps();

const positionsAfter100Steps = initPositions.map((pos, i) => move(pos, velocities[i], 100));
logIfVerboseEnv('positions after 100 steps:');
logIfVerboseEnv(draw(positionsAfter100Steps));

let part1 = saftyFactor(positionsAfter100Steps)
console.log('Part1: ', part1);


// find the longest continuos lines for each column and row
const longestLine = (positions) => {
    const positionsDict = positions.reduce((acc, pos) => {
        const key = JSON.stringify(pos);
        acc[key] = true;
        return acc;
    }, {});

    let longestRow = 0;
    for (let x = 0; x < xMax; x++) {
        let current = 0;
        for (let y = 0; y < yMax; y++) {
            if (positionsDict[JSON.stringify([x, y])]) {
                current++;
            } else {
                if (!longestRow || current > longestRow) {
                    longestRow = current;
                }
                current = 0;
            }
        }
    }
    let longestColumn = 0;
    for (let y = 0; y < yMax; y++) {
        let current = 0;
        for (let x = 0; x < xMax; x++) {
            if (positionsDict[JSON.stringify([x, y])]) {
                current++;
            } else {
                if (!longestColumn || current > longestColumn) {
                    longestColumn = current;
                }
                current = 0;
            }
        }
    }
    return [longestRow, longestColumn];
}

const countsPer10x10Grid = (positions) => {
    const counts = {};
    positions.forEach(([x, y]) => {
        const gridX = Math.floor(x / 10);
        const gridY = Math.floor(y / 10);
        if (!counts[gridY]) {
            counts[gridY] = {};
        }
        if (!counts[gridY][gridX]) {
            counts[gridY][gridX] = 0;
        }
        counts[gridY][gridX]++;
    });
    const flatCounts = Object.values(counts).reduce((acc, row) => {
        return acc.concat(Object.values(row));
    }, []);
    logIfVerboseEnv('counts per 10x10 grid:', flatCounts);
    return flatCounts;
}


let busiestGrid = 0;
for (let i = 0; i < xMax * yMax; i++) {
    const positions = initPositions.map((pos, j) => move(pos, velocities[j], i));
    const count = Math.max(...countsPer10x10Grid(positions));
    if (count > busiestGrid) {
        console.log(`Step ${i} line grid count: ${count}`);
        console.log(draw(positions, false));
        busiestGrid = count;
    }
}

let part2 = 0;
console.log('Part2: ', part2);
