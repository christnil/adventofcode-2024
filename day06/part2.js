const fs = require('fs');

const raw = fs.readFileSync('./input.txt', 'utf8')
console.log(raw)

const blocks = {};
let startPosition = [];
let startDirection = [-1, 0];
const rows = raw.split('\n');
const maxX = rows[0].length;
const maxY = rows.length;
rows.forEach((rowData, row) => {
	rowData.split('').forEach((char, column) => {
		if (char == '#') {
			if (!blocks[row]) blocks[row] = {}
			blocks[row][column] = true;
		}
		if (char == '^') startPosition = [row, column];
	});
});

const printMap = () => {
	for (let i = 0; i < maxX; i++) {
		let row = '';
		for (let j = 0; j < maxY; j++) {
			if (position[0] == i && position[1] == j) row += '^';
			else if (visited.has(JSON.stringify([i, j]))) row += 'X';
			else if (blocks[i] && blocks[i][j]) row += '#';
			else row += '.';
		}
		console.log(row);
	}
}

const solve = () => {
	let position = startPosition;
	let direction = startDirection;
	const visited = new Set();
	const positionDirectionVisited = new Set();

	const add = (pos, direction) => ([pos[0] + direction[0], pos[1] + direction[1]]);
	const turnRight = (direction) => ([1 * direction[1], -1 * direction[0]])

	visited.add(JSON.stringify(position));
	positionDirectionVisited.add(JSON.stringify([...position, ...direction]));
	
	const isLoop = (pos, dir) => positionDirectionVisited.has(JSON.stringify([...pos, ...dir]));
	const shouldBreak = (pos, dir) => {
		if (isLoop(pos, dir)) return true;
		if (pos[0] < 0 || pos[0] >= maxX) return true;
		if (pos[1] < 0 || pos[1] >= maxY) return true;
		return false;
	};

	while (true) {
		let next = add(position, direction)
		while ((blocks[next[0]] || {})[next[1]]) {
			direction = turnRight(direction);
			//console.log('New direction: ', direction);
			next = add(position, direction);
		}
		position = next;
		if (shouldBreak(position, direction))
			break;
		//console.log('New position: ', position);
		visited.add(JSON.stringify(position));
		positionDirectionVisited.add(JSON.stringify([...position, ...direction]));
	}

	return {visited, loop: isLoop(position, direction)};
}

const initialSolution = solve();
let count = 0;
initialSolution.visited.forEach(vstr => {
	const v = JSON.parse(vstr);
	if (JSON.stringify(v) != JSON.stringify(startPosition)) {
		blocks[v[0]] = blocks[v[0]] || {};
		blocks[v[0]][v[1]] = true;
		const { loop } = solve();
		if (loop) count++;
		blocks[v[0]][v[1]] = false;
	}
});

console.log("Part 2: ", count);

