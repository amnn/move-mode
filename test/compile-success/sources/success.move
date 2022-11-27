/// A nonsense module to simulate successful compilation output
module example::success {
    struct Foo {
        x: u64
    }

    public entry fun bar(foo: &mut Foo) {
        foo.x = foo.x + 1;
    }
}
