mod directions;
mod pipes;

use std::io::{stdin, BufRead};

use directions::*;
use pipes::Pipe;

// assume exactly one 'S'
fn find_start_position(inp: impl IntoIterator<Item = impl AsRef<str>>) -> (usize, usize) {
    for (y, s) in inp.into_iter().enumerate() {
        if let Some(x) = s.as_ref().find('S') {
            return (x, y);
        }
    }
    unreachable!();
}

fn is_pipe_in_direction(c: char, dir: Direction) -> bool {
    Pipe::try_from(c)
        .map(Into::into)
        .map_or(false, |(a, b)| dir == a || dir == b)
}

fn get_pipe_from_neighbors(grid: &[impl AsRef<str>], (x, y): (usize, usize)) -> Pipe {
    let get = |x: usize, y: usize| grid[y].as_ref().chars().nth(x).unwrap();
    let (w, h) = (grid[0].as_ref().len(), grid.len());
    let left = x != 0 && is_pipe_in_direction(get(x - 1, y), RIGHT);
    let right = x != w - 1 && is_pipe_in_direction(get(x + 1, y), LEFT);
    let up = y != 0 && is_pipe_in_direction(get(x, y - 1), DOWN);
    let down = y != h - 1 && is_pipe_in_direction(get(x, y + 1), UP);
    let c = match (left, right, up, down) {
        (true, true, false, false) => '-',
        (true, false, true, false) => 'J',
        (true, false, false, true) => '7',
        (false, true, true, false) => 'L',
        (false, true, false, true) => 'F',
        (false, false, true, true) => '|',
        _ => panic!("left={left}, right={right}, up={up}, down={down}"),
    };
    c.try_into().unwrap()
}

fn get_loop(grid: &[impl AsRef<str>]) -> Vec<(usize, usize)> {
    let start_pos = find_start_position(grid);
    let (sdir, _) = get_pipe_from_neighbors(grid, start_pos).into();
    let fst_pos = move_in_dir(start_pos, sdir);
    let mut res = vec![start_pos, fst_pos];
    while *res.last().unwrap() != start_pos {
        let (x, y) = res[res.len() - 1];
        let last = res[res.len() - 2];
        let c = grid[y].as_ref().chars().nth(x).unwrap();
        let (a, b) = Pipe::try_from(c).unwrap().into();
        let next = match move_in_dir((x, y), a) {
            new if new != last => new,
            _ => move_in_dir((x, y), b),
        };
        res.push(next);
    }
    res
}

// https://en.wikipedia.org/wiki/Shoelace_formula
fn double_area_of_loop_polygon(loop_verts: &[(usize, usize)]) -> usize {
    let get = |i| match i {
        0 => loop_verts[loop_verts.len() - 1],
        i if i <= loop_verts.len() => loop_verts[i - 1],
        _ /* n+1 */ => loop_verts[0],
    };
    let sgn: fn((usize, usize)) -> (isize, isize) = |(a, b)| (a.try_into().unwrap(), b.try_into().unwrap());
    let get = |i| sgn(get(i));
    let sum = (1..=loop_verts.len())
        .map(|i| {
            let (xi, yi) = get(i);
            let (xj, yj) = get(i + 1);
            (yi + yj) * (xi - xj)
        })
        .sum::<isize>();
    std::cmp::max(sum, -sum).try_into().unwrap()
}

// https://en.wikipedia.org/wiki/Pick%27s_theorem
fn interior_points_of_loop(loop_verts: &[(usize, usize)]) -> usize {
    let a2 = double_area_of_loop_polygon(loop_verts);
    (a2 + 2 - (loop_verts.len() - 1)) / 2
}

fn main() {
    let grid: Vec<_> = stdin().lock().lines().map(Result::unwrap).collect();
    assert_ne!(grid.len(), 0);
    assert!(grid.iter().all(|line| line.len() == grid[0].len()));
    let loop_positions = get_loop(&grid);
    println!("Part 1: {}", loop_positions.len() / 2);
    println!("Part 2: {}", interior_points_of_loop(&loop_positions));
}
