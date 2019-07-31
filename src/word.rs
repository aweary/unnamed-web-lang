pub struct Word {
  pub text: String,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Reserved {
  Const,
  Component,
  Function,
  Null,
  True,
  False,
  Let,
  If,
  Else,
  Return,
}
