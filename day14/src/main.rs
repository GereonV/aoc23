use std::{
    cmp,
    collections::HashMap,
    io::{stdin, BufRead},
    mem::swap,
};

#[derive(Debug, Clone)]
struct Solver {
    map: Vec<String>,
}

impl Solver {
    fn read() -> Self {
        let map = stdin()
            .lock()
            .lines()
            .map(Result::unwrap)
            .collect::<Vec<_>>();
        assert_ne!(map.len(), 0);
        assert!(map.iter().all(|x| x.len() == map[0].len()));
        Self { map }
    }

    fn get(&self, x: usize, y: usize) -> char {
        self.map[y].chars().nth(x).unwrap()
    }

    fn swap(&mut self, a: (usize, usize), b: (usize, usize)) {
        let ((ax, ay), (bx, by)) = (a, b);
        match Ord::cmp(&ay, &by) {
            cmp::Ordering::Equal => {
                if ax != bx {
                    let line = unsafe { self.map[ay].as_bytes_mut() };
                    line.swap(ax, bx);
                }
            }
            cmp::Ordering::Less => {
                let (sa, sb) = self.map.split_at_mut(by);
                let sa = unsafe { sa[ay].as_bytes_mut() };
                let sb = unsafe { sb[0].as_bytes_mut() };
                swap(&mut sa[ax], &mut sb[bx]);
            }
            cmp::Ordering::Greater => {
                let (sb, sa) = self.map.split_at_mut(ay);
                let sa = unsafe { sa[0].as_bytes_mut() };
                let sb = unsafe { sb[by].as_bytes_mut() };
                swap(&mut sa[ax], &mut sb[bx]);
            }
        }
    }

    fn part1(&self) -> usize {
        (0..self.map[0].len())
            .map(|x| {
                (0..self.map.len())
                    .fold((0, 0), |(free_pos, acc), y| match self.get(x, y) {
                        '.' => (free_pos, acc),
                        '#' => (y + 1, acc),
                        'O' => (free_pos + 1, acc + self.map.len() - free_pos),
                        _ => unreachable!(),
                    })
                    .1
            })
            .sum()
    }

    fn part2(mut self) -> usize {
        const CYCLES: usize = 1_000_000_000;
        let (width, height) = (self.map[0].len(), self.map.len());
        let mut hm = HashMap::new();
        for idx in 0..CYCLES {
            if let Some(fst_idx) = hm.insert(self.map.clone(), idx) {
                let remaining = CYCLES - idx;
                let loop_size = idx - fst_idx;
                let needle = fst_idx + remaining % loop_size;
                self.map = hm.into_iter().find(|x| x.1 == needle).unwrap().0;
                break;
            }
            for x in 0..width {
                let mut free = 0;
                for y in 0..height {
                    match self.get(x, y) {
                        '.' => (),
                        '#' => free = y + 1,
                        'O' => {
                            self.swap((x, y), (x, free));
                            free += 1;
                        }
                        _ => unreachable!(),
                    }
                }
            }
            for y in 0..height {
                let mut free = 0;
                for x in 0..width {
                    match self.get(x, y) {
                        '.' => (),
                        '#' => free = x + 1,
                        'O' => {
                            self.swap((x, y), (free, y));
                            free += 1;
                        }
                        _ => unreachable!(),
                    }
                }
            }
            for x in 0..width {
                let mut free = height - 1;
                for y in (0..height).rev() {
                    match self.get(x, y) {
                        '.' => (),
                        '#' => {
                            if y != 0 {
                                free = y - 1;
                            }
                        }
                        'O' => {
                            self.swap((x, y), (x, free));
                            free -= 1;
                        }
                        _ => unreachable!(),
                    }
                }
            }
            for y in 0..height {
                let mut free = width - 1;
                for x in (0..width).rev() {
                    match self.get(x, y) {
                        '.' => (),
                        '#' => {
                            if x != 0 {
                                free = x - 1;
                            }
                        }
                        'O' => {
                            self.swap((x, y), (free, y));
                            free -= 1;
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }
        (0..self.map[0].len())
            .map(|x| {
                (0..self.map.len())
                    .map(|y| match self.get(x, y) {
                        'O' => self.map.len() - y,
                        _ => 0,
                    })
                    .sum::<usize>()
            })
            .sum()
    }
}

fn main() {
    let solver = Solver::read();
    println!("Part 1: {}", solver.part1());
    println!("Part 2: {}", solver.part2());
}
