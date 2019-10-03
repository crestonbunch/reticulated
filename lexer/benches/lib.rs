#[macro_use]
extern crate criterion;

use criterion::Criterion;
use lexer::buffer::TokenBuffer;
use lexer::Lexer;

fn bench_tokenizing(b: &mut Criterion) {
    b.bench_function("tokenize_strings", |b| {
        let inputs = vec!["\"foo\"", "'foo'", "'''foo'''", "\"\"\"foo\"\"\""];
        for input in inputs {
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);
            b.iter(|| lexer.next())
        }
    });
    b.bench_function("tokenize_simple_function", |b| {
        b.iter(|| {
            let input = "def test_func(param: int, **kwargs) -> int: \
                         \n\treturn param**2";
            let input = TokenBuffer::new(input.as_bytes());
            let lexer = &mut Lexer::new(input);
            let mut i = 0;
            for _ in lexer {
                i += 1;
            }
            i
        })
    });
}

criterion_group!(benches, bench_tokenizing);
criterion_main!(benches);
