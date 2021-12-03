import {promises as fs} from 'fs';
import util from 'util';

const lines = (await fs.readFile('./input.txt'))
			.toString()
			.split('\n');

const result = lines
			.map(x => [...x].map(x => parseInt(x, 2)))
			.reduce((o, v) => {
				for (let i = 0; i < v.length; i++) {
					if (o[i] === undefined) {
						o[i] = 0;
					}
					if (v[i] === 1) {
						o[i] += 1;
					} else {
						o[i] -= 1;
					}
				}
				return o;
			}, [])
			.map(v => v >= 0 ? 1 : 0);

const resultInt = parseInt(result.join(""), 2);
const inverseInt = parseInt(
	result.map(v => v == 1 ? 0 : 1).join(""),
	2
);
const part1 = resultInt * inverseInt;
console.log(`part1: ${part1}`);

const update = ({count, children}, v) => {
	count += 1;
	if (v.length > 0) {
		const	value = v[0];
		if (children[value] === undefined) {
			children[value] = {count: 0, children: {}};
		}
		children[value] = update(children[value], v.slice(1));
	}
	return {count, children};
};

const tree = lines.reduce(update, {count: 0, children: {}});

const trace = ({children}) => {
	if (Object.keys(children).length === 0) {
		return "";
	}
	const key = Object.keys(children)[0];
	return key + trace(children[key]);
};

const walkMaxOr1 = ({count, children}) => {
	if (count === 1) {
		return trace({children});
	}
	const count0 = children["0"]?.count ?? 0;
	const count1 = children["1"]?.count ?? 0;
	if (count1 > count0) {
		return "1" + walkMaxOr1(children["1"]);
	} else if (count0 > count1) {
		return "0" + walkMaxOr1(children["0"]);
	} else if (count1 === 0 && count0 === 0) {
		return "";
	}
	return "1" + walkMaxOr1(children["1"]);
};

const walkMinOr0 = ({count, children}) => {
	if (count === 1) {
		return trace({children});
	}
	const count0 = children["0"]?.count ?? 0;
	const count1 = children["1"]?.count ?? 0;
	if (count1 > count0) {
		return "0" + walkMinOr0(children["0"]);
	} else if (count0 > count1) {
		return "1" + walkMinOr0(children["1"]);
	} else if (count1 === 0 && count0 === 0) {
		return "";
	}
	return "0" + walkMinOr0(children["0"]);
};

const oxygen = parseInt(walkMaxOr1(tree), 2);
const scrubber = parseInt(walkMinOr0(tree), 2);
const part2 = oxygen * scrubber;

console.log(`part2: ${part2}`);
