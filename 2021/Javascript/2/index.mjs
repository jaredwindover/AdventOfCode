import { promises as fs } from "fs";

const input = (await fs.readFile("./input.txt"))
  .toString()
  .split("\n")
  .map((line) => line.split(" "))
  .map(([command, value]) => ({ command, value: parseInt(value) }));

const part1 = input.reduce(
  ({ hor, depth }, { command, value }) => {
    switch (command) {
      case "forward":
        hor += value;
        break;
      case "down":
        depth += value;
        break;
      case "up":
        depth -= value;
        break;
    }
    return { hor, depth };
  },
  { hor: 0, depth: 0 }
);

const part2 = input.reduce(
  ({ hor, depth, aim }, { command, value }) => {
    switch (command) {
      case "forward":
        hor += value;
        depth += value * aim;
        break;
      case "down":
        aim += value;
        break;
      case "up":
        aim -= value;
        break;
    }
    return { hor, depth, aim };
  },
  { hor: 0, depth: 0, aim: 0 }
);

console.log(part1);
console.log(part2);
console.log(part2.hor * part2.depth);
