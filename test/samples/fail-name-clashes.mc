interface Banana {
  def foo(a: int);
}

interface Apple {
  def foo(a: int);
}

component A provides Apple {
  def foo(a: int) {
    return;
  }
}

component B provides Banana {
  def foo(a: int) {
    return;
  }
}

component EntryPoint provides App uses Banana, Apple {
  def main() : int {

    foo(2);
    foo(1);

    print(42);
    print(71);
    print(1);
    return 0;
  }
}

connect {
  EntryPoint.Banana <- B.Banana;
  EntryPoint.Apple <- A.Apple;
}