module example::warn {
    public entry fun bar(x: u64, y: u64) {
        let _z = x + 1;
    }
}
