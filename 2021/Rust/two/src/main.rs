use nom::character::complete::{alpha1, space0, u32};
use nom::sequence::separated_pair;
use nom::IResult;
use std::io;
use std::io::prelude::*;
use strum_macros::EnumString;

#[derive(Debug, PartialEq, EnumString)]
enum Command {
    #[strum(serialize = "forward")]
    Forward,
    #[strum(serialize = "up")]
    Up,
    #[strum(serialize = "down")]
    Down,
}

fn main() {
    // let mut command = alt((tag("forward"), tag("up"), tag("down")));
    let stdin = io::stdin();
    let (x, y) = stdin
        .lock()
        .lines()
        .map(|x| {
            let string = x.unwrap();
            let res: IResult<&str, (&str, u32)> = separated_pair(alpha1, space0, u32)(&string[..]);
            let raw = res.unwrap();
            let command: Command = raw.1 .0.parse().unwrap();
            let value = raw.1 .1;
            (command, value)
        })
        .fold((0, 0), |(x, y), (command, value)| match command {
            Command::Forward => (x + value, y),
            Command::Down => (x, y + value),
            Command::Up => (x, y - value),
        });
    println!("{}", x * y);
}
