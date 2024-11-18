use super::directions::*;
use std::fmt::Display;

#[derive(Debug)]
pub struct PipeError;
impl std::error::Error for PipeError {}
impl Display for PipeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct Pipe(Direction, Direction);

impl From<Pipe> for (Direction, Direction) {
    fn from(value: Pipe) -> Self {
        let Pipe(a, b) = value;
        (a, b)
    }
}

impl TryFrom<char> for Pipe {
    type Error = PipeError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        let (a, b) = match value {
            '|' => (DOWN, UP),
            '-' => (LEFT, RIGHT),
            'L' => (RIGHT, UP),
            'J' => (LEFT, UP),
            '7' => (LEFT, DOWN),
            'F' => (DOWN, RIGHT),
            _ => return Err(PipeError),
        };
        Ok(Self(a, b))
    }
}
