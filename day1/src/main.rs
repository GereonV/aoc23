use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Sum;
use regex::Regex;

const DIGITS: [&str; 9] = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];

struct Vec2(usize, usize);
impl Sum<(usize, usize)> for Vec2 {
    fn sum<I: Iterator<Item = (usize, usize)>>(i: I) -> Self {
        let mut x = (0, 0);
        for (a, b) in i {
            x = (x.0 + a, x.1 + b);
        }
        Vec2(x.0, x.1)
    }
}

fn main() {
    let mut map = HashMap::with_capacity(19);
    (0..=9).for_each(|i| { map.insert(i.to_string(), i); });
    DIGITS.iter().enumerate().for_each(|(i, d)| { map.insert(d.to_string(), i + 1); });
    let re = Regex::new(&DIGITS.iter().fold(r"\d".to_string(), |s, d| s + "|" + d)).unwrap();
    let f = File::open("input").expect("file 'input' should exist");
    let Vec2(s1, s2) = BufReader::new(f).lines().map(
        |l| {
            let l = l.expect("reading should not fail");
            let first1 = l. find(|c: char| c.is_ascii_digit()).expect("line should contain digit");
            let  last1 = l.rfind(|c: char| c.is_ascii_digit()).expect("line should contain digit");
            let mut matches = re.find_iter(&l).map(|m| map.get(m.as_str()).expect("digits should be in map"));
            let first2 = matches.next().expect("line should contain digit");
            let  last2 = matches.last().unwrap_or(first2);
            let b = l.as_bytes();
            let first1 = (b[first1] - b'0') as usize;
            let  last1 = (b[ last1] - b'0') as usize;
            (10 * first1 + last1, 10 * first2 + last2)
        }
    ).sum();
    println!("Solution 1: {s1}");
    println!("Solution 2: {s2}");
}
