use std::ops::{ControlFlow, FromResidual, Try};

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

pub enum UnifyResult {
    Ok,
    CantUnify,
    Cyclic,
}

impl Try for UnifyResult {
    type Output = Self;
    type Residual = Self;

    fn from_output(output: Self::Output) -> Self {
        output
    }

    fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
        match self {
            Self::Ok => ControlFlow::Continue(self),
            _ => ControlFlow::Break(Self::Cyclic),
        }
    }
}

impl FromResidual for UnifyResult {
    fn from_residual(residual: <Self as Try>::Residual) -> Self {
        residual
    }
}
