pub fn format_radix(mut x: usize, radix: usize) -> String {
    let mut result = vec![];
    loop {
        let m = x % radix;
        x = x / radix;
        result.push(std::char::from_u32((m + 96) as u32).unwrap());
        if x == 0 {
            break;
        }
    }
    format!("'{}", result.into_iter().rev().collect::<String>())
}