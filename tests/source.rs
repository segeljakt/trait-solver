


#[test]
fn test_source0() {
    let mut s = aqua::diag::Sources::new();
    assert_eq!(s.add("file0", "val x = 0;"), 0);
    assert_eq!(s.add("file1", "def f() = 1;"), 1);
}
