const fs = require('fs');

const raw = fs.readFileSync('./input.txt', 'utf8')
const rows = raw.split('\n')


const logIfVerboseEnv = (...args) => {
  if (process.env.VERBOSE) {
    console.log(...args);
    }
};

const edges = rows
    .map(row => {
        const [a, b] = row.split('-');
        return [a, b];
    })
    .reduce((acc, [a, b]) => {
        if (!acc[a]) {
            acc[a] = new Set();
        }
        if (!acc[b]) {
            acc[b] = new Set();
        }
        acc[a].add(b);
        acc[b].add(a);
        return acc;
    }, {});

const vertices = new Set(Object.keys(edges));

const part1 = () => {
    const key = (a, b, c) => [a, b, c].sort().join('-');
    const tripples = new Set();
    [...vertices]
    .filter(v => v.startsWith('t'))
    .forEach(v1 => {
        edges[v1].forEach(v2 => {
            edges[v2].forEach(v3 => {
                if (edges[v3].has(v1)) {
                    tripples.add(key(v1, v2, v3));
                }
            });
        });
    });
    return tripples.size;
};

console.log('Part 1: ', part1())
