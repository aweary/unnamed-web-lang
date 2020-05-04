use serde::{Deserialize, Serialize};

#[derive(
    Debug, PartialOrd, Ord, PartialEq, Eq, Clone, Copy, Serialize, Deserialize,
)]
pub enum Precedence {
    NONE = 0,
    ASSIGNMENT = 1,
    CONDITIONAL = 2,
    SUM = 3,
    PRODUCT = 4,
    COMPARE = 5,
    PREFIX = 6,
}
