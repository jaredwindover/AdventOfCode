use itertools::Itertools;
use std::io;
use std::io::prelude::*;

fn main() {
    let stdin = io::stdin();
    let input_iter = stdin
        .lock()
        .lines()
        .map(|x| x.unwrap().parse::<u32>().unwrap());
    // let first = input_iter.next().unwrap();
    // let count_increases = input_iter.fold((first, 0), |(so_far, count), value| {
    //     (value, count + if value > so_far { 1 } else { 0 })
    // });
    // println!("part1: {}", count_increases.1);

    let mut windowed = input_iter.tuple_windows().map(|(a, b, c)| a + b + c);
    let first = windowed.next();
    let count_increases = windowed.fold((first.unwrap(), 0), |(so_far, count), value| {
        (value, count + if value > so_far { 1 } else { 0 })
    });
    println!("part2: {}", count_increases.1)
}
