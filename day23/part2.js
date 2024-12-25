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

const part2 = () => {
    const cliques = []

    const bronKerbosch = (R, P, X) => {
        if (P.size === 0 && X.size === 0) {
            cliques.push(R);
            return;
        }
        [...P].forEach(v => {
            const newR = new Set(R);
            newR.add(v);
            const newP = new Set([...P].filter(x => edges[v].has(x) && [...newR].every(r => edges[x].has(r))));
            const newX = new Set([...X].filter(x => edges[v].has(x) && [...newR].every(r => edges[x].has(r))));
            bronKerbosch(newR, newP, newX);
            P.delete(v);
            X.add(v);
        });
    }

    bronKerbosch(new Set(), vertices, new Set());

    const largestClique = cliques.reduce((acc, c) => c.size > acc.size ? c : acc, new Set());
    console.log('Largest clique size: ', largestClique.size);
    const password = [...largestClique].sort().join(',');
    return password
};

console.log('Part 2: ', part2())
