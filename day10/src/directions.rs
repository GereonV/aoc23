use std::fmt::Display;

#[derive(Debug)]
pub struct DirectionError;
impl std::error::Error for DirectionError {}
impl Display for DirectionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Direction(u8);
pub const LEFT: Direction = Direction(0);
pub const DOWN: Direction = Direction(1);
pub const RIGHT: Direction = Direction(2);
pub const UP: Direction = Direction(3);
const DIRECTIONS: [(isize, isize); 4] = [
    (-1, 0), // left
    (0, 1),  // down
    (1, 0),  // right
    (0, -1), // up
];

pub fn move_in_dir((x, y): (usize, usize), dir: Direction) -> (usize, usize) {
    let (dx, dy) = dir.into();
    (
        (x as isize + dx).try_into().unwrap(),
        (y as isize + dy).try_into().unwrap(),
    )
}

impl Direction {
    pub fn opposite(&self) -> Self {
        Self(self.0 ^ 2)
    }
}

impl From<Direction> for (isize, isize) {
    fn from(value: Direction) -> Self {
        DIRECTIONS[usize::from(value.0)]
    }
}

impl TryFrom<u8> for Direction {
    type Error = DirectionError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match usize::from(value) < DIRECTIONS.len() {
            true => Ok(Self(value)),
            false => Err(DirectionError),
        }
    }
}
