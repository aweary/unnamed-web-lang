use criterion::{criterion_group, criterion_main, Criterion};
use lexer::Lexer;

const SOURCE: &str = include_str!("fixtures/lexer_small_file.dan");

fn lex_small_string() {
    let mut lexer = Lexer::new(SOURCE);
    let mut tokens = vec![];
    while let Ok(token) = lexer.next_token() {
        if token.kind == syntax::token::TokenKind::EOF {
            break;
        }
        tokens.push(token)
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("lex_small_string", |b| b.iter(lex_small_string));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
