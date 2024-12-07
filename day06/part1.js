const fs = require('fs');

const raw = fs.readFileSync('./input.txt', 'utf8')
console.log(raw)

const blocks = {};
let position = [];
let direction = [-1, 0];
const rows = raw.split('\n');
const maxX = rows[0].length;
const maxY = rows.length;
rows.forEach((rowData, row) => {
	rowData.split('').forEach((char, column) => {
		if (char == '#') {
			if (!blocks[row]) blocks[row] = {}
			blocks[row][column] = true;
		}
		if (char == '^') position = [row, column];
	});
});

const visited = new Set();
const positionDirectionVisited = new Set();

const add = (pos, direction) => ([pos[0] + direction[0], pos[1] + direction[1]]);
const turnRight = (direction) => ([1 * direction[1], -1 * direction[0]])

visited.add(JSON.stringify(position));
positionDirectionVisited.add(JSON.stringify([...position, ...direction]));

const shouldBreak = (pos, dir) => {
	if (positionDirectionVisited.has(JSON.stringify([...pos, ...dir]))) return true;
	if (pos[0] < 0 || pos[0] >= maxX) return true;
	if (pos[1] < 0 || pos[1] >= maxY) return true;
	return false;
};

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

while (true) {
	let next = add(position, direction)
	while ((blocks[next[0]] || {})[next[1]]) {
		direction = turnRight(direction);
		//console.log('New direction: ', direction);
		next = add(position, direction);
	}
	if (shouldBreak(next, direction))
		break;
	position = next;
	//console.log('New position: ', position);
	visited.add(JSON.stringify(position));
	positionDirectionVisited.add(JSON.stringify([...position, ...direction]));
}

printMap()

console.log("Part 1: ", visited.size);

