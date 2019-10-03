use string_interner::{StringInterner, Sym};

// "Global" context used for a single parsing session
pub struct ParsingContext {
    pub interner: StringInterner<Sym>,
}

impl ParsingContext {
    pub fn new() -> Self {
        Self {
            interner: StringInterner::default(),
        }
    }

    pub fn report_err(&self, message: &str) {
        println!("report error: {}", message)
    }

}
