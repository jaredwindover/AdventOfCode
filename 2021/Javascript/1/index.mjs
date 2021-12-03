import { promises as fs } from "fs";

const data = await fs.readFile("./input.txt");
const lines = data
  .toString()
  .split("\n")
  .slice(0, -1)
  .map((x) => parseInt(x, 10));
const INF = Symbol("Infinite");

const countInstancesOfIncrease = (lines) =>
  lines.reduce(
    ({ prev, count }, v) => {
      if (prev !== INF && v > prev) {
        count += 1;
      }
      return { prev: v, count };
    },
    { prev: INF, count: 0 }
  );

const slidingWindow = (lines) =>
  lines
    .reduce(
      ({ a, b, c, results }, v) => {
        // console.log({a,b,c, v, results});
        b += v;
        c += v;
        results.push(b);
        return {
          a: b,
          b: c,
          c: v,
          results,
        };
      },
      { a: 0, b: 0, c: 0, results: [] }
    )
    .results.slice(2);
// console.log(lines);
// console.log(slidingWindow(lines.slice(0, 20)));

// console.log(countInstancesOfIncrease(lines));

console.log(countInstancesOfIncrease(slidingWindow(lines)));
