/// A nonsense module to test syntax highlighting, indentation,
/// interaction with the CLI, etc.  Not expected to build.
module example::test {
    use acme::widget::{Self, Widget};
    use std::vector;

    const EAddressMismatch: u64 = 1;

    /**
     * Lorem ipsum dolors set amet, consectetur adipiscing elit.
     */
    struct Foo<T: drop> has drop {
        bar: address,
        baz: T,
        qux: vector<u8>,
        quy: u64,
    }

    /// Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.
    public entry fun quz<T: drop + store>(f: &mut Foo<T>, w: &mut Widget): &T {
        assert!(w.bar == w.addr, EAddressMismatch);
        widget::twiddle(w, vector::length(&f.qux));

        let i = 0;
        let sum_with_a_really_long_name = 0;
        
        while (i < vector::length(&f.qux)) {
            sum_with_a_really_long_name = sum_with_a_really_long_name
                + vector::borrow(&f.qux, i);
            i = i + 1;
        };
        
        // Ut enim ad minim veniam.
        f.quy = f.quy + 42u64 + vector::length(f.qux)
            + sum_with_a_really_long_name;

        return &f.baz
    }
}
