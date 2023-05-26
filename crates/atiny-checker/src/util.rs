use std::fmt::Display;

pub fn format_radix(mut x: usize, radix: usize) -> String {
    let mut result = vec![];
    loop {
        let m = x % radix;
        x /= radix;
        result.push(std::char::from_u32((m + 96) as u32).unwrap());
        if x == 0 {
            break;
        }
    }
    format!("'{}", result.into_iter().rev().collect::<String>())
}

#[derive(Debug)]
pub struct OccursCheck;

impl Display for OccursCheck {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "found cyclic type of infinite size")
    }
}
