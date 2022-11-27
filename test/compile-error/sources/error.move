module example::error {
    use example::foo::Foo;

    public entry fun foo(x: u64, y: Foo) {
        let z = x + y;
    }
}
