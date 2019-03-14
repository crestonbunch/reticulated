#[macro_use]
extern crate criterion;

use criterion::Criterion;
use lexer::buffer::TokenBuffer;
use std::iter::repeat;

fn bench_scanning(b: &mut Criterion) {
    b.bench_function("scan_forward", |b| {
        let input: String = repeat("A").take(5000).collect();
        let mut buf = TokenBuffer::new(input.as_bytes());
        b.iter(|| {
            for _ in 0..5000 {
                buf.scan_forward();
            }
        })
    });
    b.bench_function("scan_forward_surrogates", |b| {
        let input: String = repeat("𐐷").take(1250).collect();
        let mut buf = TokenBuffer::new(input.as_bytes());
        b.iter(|| {
            for _ in 0..1250 {
                buf.scan_forward();
            }
        })
    });
}

fn bench_popping(b: &mut Criterion) {
    b.bench_function("pop_four_times", |b| {
        let input: String = repeat("A").take(20000).collect();
        let mut buf = TokenBuffer::new(input.as_bytes());
        b.iter(|| {
            for _ in 0..4 {
                for _ in 0..5000 {
                    buf.scan_forward();
                }
                buf.pop();
            }
        })
    });

    b.bench_function("pop_100_times", |b| {
        let input: String = repeat("A").take(20000).collect();
        let mut buf = TokenBuffer::new(input.as_bytes());
        b.iter(|| {
            for _ in 0..100 {
                for _ in 0..200 {
                    buf.scan_forward();
                }
                buf.pop();
            }
        })
    });

    b.bench_function("pop_1000_times", |b| {
        let input: String = repeat("A").take(20000).collect();
        let mut buf = TokenBuffer::new(input.as_bytes());
        b.iter(|| {
            for _ in 0..1000 {
                for _ in 0..20 {
                    buf.scan_forward();
                }
                buf.pop();
            }
        })
    });
}

criterion_group!(benches, bench_scanning, bench_popping);
criterion_main!(benches);
