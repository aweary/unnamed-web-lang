use syntax::Span;


pub type CheckResult<T> = std::result::Result<T, (String, Span)>;
