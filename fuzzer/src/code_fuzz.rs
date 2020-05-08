use quickcheck::{Arbitrary, Gen};

use rand::distributions::{Distribution, Standard};
use rand::seq::SliceRandom;
use rand::{thread_rng, Rng};

use fxhash::FxHashSet;

#[derive(Debug)]
struct IdentDistribution;

impl Distribution<char> for IdentDistribution {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> char {
        const RANGE: u32 = 26 + 26 + 2;
        const VALID_ASCII_IDENT_CHARS: &[u8] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                abcdefghijklmnopqrstuvwxyz\
                $_";
        loop {
            let var = rng.next_u32() >> (32 - 6);
            if var < RANGE {
                return VALID_ASCII_IDENT_CHARS[var as usize] as char;
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum FuzzStmtChoice {
    // A local, let binding
    Local,
    // An item definition, local to some block
    Item,
    // Expression statement
    // TODO should we differentiate expressions with or without semicolons?
    Expr,
    // A while loop
    While,
    // If statement
    If,
    // Return statement
    Return,
    // Try/catch statement
    TryCatch,
}

/// Allow randomly generating a `FuzzStmtChoice` across a standard distribution
impl Distribution<FuzzStmtChoice> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> FuzzStmtChoice {
        match rng.gen_range(0, 6) {
            0 => FuzzStmtChoice::Local,
            1 => FuzzStmtChoice::Item,
            2 => FuzzStmtChoice::Expr,
            3 => FuzzStmtChoice::While,
            4 => FuzzStmtChoice::If,
            5 => FuzzStmtChoice::Return,
            6 => FuzzStmtChoice::TryCatch,
            _ => unreachable!(),
        }
    }
}

#[derive(Default, Clone, Debug)]
pub struct CodeFuzzerRestrictions {
    stmt_restrictions: Option<Vec<FuzzStmtChoice>>,
}

impl CodeFuzzerRestrictions {
    pub fn stmts(&self) -> &Option<Vec<FuzzStmtChoice>> {
        &self.stmt_restrictions
    }

    pub fn set_stmt_restrictions(&mut self, restrictions: Vec<FuzzStmtChoice>) {
        self.stmt_restrictions = Some(restrictions);
    }
}

/// Generates random valid code snippets
#[derive(Default, Clone, Debug)]
pub struct CodeFuzzer {
    /// The code that has already been generated
    /// for this instance.
    code: String,
    /// In some instances we want to restrict which kind
    /// of code we are generating. `CodeFuzzerRestrictions`
    /// encapsulates that state and provides an API for interacting with it
    restrictions: CodeFuzzerRestrictions,
    /// Set of already used identifiers, ensures we only generate unique ident names
    used_idents: FxHashSet<String>,
    /// Tracking horizontal offset for spaces/tabs
    tab_depth: usize,
}

/// This lets us use quickcheck for tests that rely on
/// parsing code samples
impl Arbitrary for CodeFuzzer {
    fn arbitrary<G: Gen>(_gen: &mut G) -> Self {
        let mut fuzz = CodeFuzzer::default();
        fuzz.restrict_stmts(vec![
            FuzzStmtChoice::Local,
            FuzzStmtChoice::If,
            FuzzStmtChoice::While,
            FuzzStmtChoice::Return,
        ]);
        // let line_count = gen.next_u32() >> 27;
        fuzz.gen_lines(10);
        fuzz
    }
}

impl CodeFuzzer {
    /// Get the code that has been generated
    pub fn code(&self) -> &str {
        &self.code
    }

    /// Restrict the fuzzer to some subset of statement choices
    pub fn restrict_stmts(&mut self, restrictions: Vec<FuzzStmtChoice>) {
        self.restrictions.set_stmt_restrictions(restrictions);
    }

    /// Generate `line_count` number of random lines
    pub fn gen_lines(&mut self, line_count: usize) {
        for _ in 0..line_count {
            self.gen_stmt();
        }
    }

    /// Randomly choose the type of statement to generate,
    /// respecting any restrictions that have been enabled.
    fn choose_stmt_kind(&self) -> FuzzStmtChoice {
        if let Some(restrictions) = self.restrictions.stmts() {
            let kind = restrictions.choose(&mut rand::thread_rng()).unwrap();
            kind.clone()
        } else {
            rand::random()
        }
    }

    /// Push some code into the `self.code`.
    fn commit(&mut self, code: &str) {
        self.code.push_str(code);
    }

    fn commit_seq(&mut self, code_seq: Vec<&str>) {
        for code in code_seq {
            self.code.push_str(code);
        }
    }

    /// Generate a random identifier
    fn gen_ident(&self) -> String {
        loop {
            let ident: String = thread_rng()
                .sample_iter(&IdentDistribution)
                .take(3)
                .collect();
            if !self.used_idents.contains(&ident) {
                return ident;
            }
        }
    }

    fn gen_expr(&mut self) -> String {
        // Number literal
        if rand::random() {
            format!("{}", rand::random::<u16>())
        } else if rand::random() {
            // String literal
            format!("\"{}\"", self.gen_ident())
        } else {
            // Binary addition
            let left = self.gen_expr();
            let right = self.gen_expr();
            format!("{} + {}", left, right)
        }
        // TODO we only support number literals now
    }

    fn gen_local(&mut self) -> String {
        let name = self.gen_ident();
        let value = self.gen_expr();
        format!("let {} = {}", name, value)
    }

    /// Generate a single statement
    fn gen_stmt(&mut self) {
        use FuzzStmtChoice::{If, Local, Return, While};
        match self.choose_stmt_kind() {
            // A local definition
            Local => {
                let ident = self.gen_ident();
                let value = self.gen_expr();
                self.commit_seq(vec!["let ", &ident, " = ", &value]);
                self.commit(";\n");
            }
            // An if block
            If => {
                let condition = self.gen_expr();
                // Condition and opening block
                self.commit_seq(vec!["if (", &condition, ") {"]);
                self.tab_depth += 1;
                let d: f64 = (self.tab_depth as f64) / 150.0;
                let c = thread_rng().gen_range(0.0, 1.0);
                if c > d {
                    self.gen_lines(thread_rng().gen_range(0, 5));
                }
                self.tab_depth -= 1;
                self.commit("}");
                self.commit(";\n");
            }
            While => {
                let condition = self.gen_expr();
                self.commit_seq(vec!["while (", &condition, ") {"]);
                self.tab_depth += 1;
                let d: f64 = (self.tab_depth as f64) / 150.0;
                let c = thread_rng().gen_range(0.0, 1.0);
                if c > d {
                    self.gen_lines(thread_rng().gen_range(0, 5));
                }
                self.tab_depth -= 1;
                self.commit("}");
                self.commit(";\n");
            }
            Return => {
                let value = self.gen_expr();
                self.commit_seq(vec!["return ", &value]);
                self.commit(";\n");
            }
            _ => (),
        }
    }
}
