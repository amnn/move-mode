/// This module tests highlighting for new syntax introduced as part
/// of Move 2024.
module example::test {
    use acme::{widget::Widget, gadget};

    #[error]
    const EAddressMismatch: u64 = 1;

    /**
     * Lorem ipsum dolors set amet, consectetur adipiscing elit.
     */
    public struct Foo<T: drop> has drop {
        bar: address,
        baz: T,
        qux: vector<Baz>,
        quy: u64,
    }

    public struct Bar(u32, u64) has copy, drop, store;

    public enum Baz has drop, store {
        Qux(u32),
        Quy(Bar),
    }

    /// Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
    public fun quz<T: drop + store>(f: &mut Foo<T>, w: &mut Widget): &T {
        assert!(w.bar == w.addr, EAddressMismatch);
        w.twiddle(f.qux.length());

        let mut i = 0;
        let mut sum_with_a_really_long_name = 0;

        'outer: while (i < f.qux.length()) {
            if (i % 42 == 0) {
                continue 'outer;
            };

            match (f.qux[i]) {
                Baz::Qux(x) => {
                    sum_with_a_really_long_name = sum_with_a_really_long_name
                        + (x as u64);
                },
                Baz::Quy(Bar(y, z)) => {
                    sum_with_a_really_long_name = sum_with_a_really_long_name
                        + (y as u64) + z;
                },
            };

            i = i + 1;
        };

        // Ut enim ad minim veniam.
        f.quy = f.quy + 42u64 + f.qux.length()
            + sum_with_a_really_long_name;

        return &f.baz
    }
}
