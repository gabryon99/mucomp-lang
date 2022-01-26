component EntryPoint provides App { 

  def main() : int {

    // Unit test for additional operators:
    // ++, -- (pre and post)
    // +=, -=, /=, *=, %=

    var a: int = 16;
    var b: float = 3.14;

    a++;
    print(a); // 17
    ++a;
    print(a); // 18

    print(++a + 2); // 21
    print(a); // 19
    print(a++ + 1); // 20
    print(a); //  20

    a--;
    print(a); // 19
    --a;
    print(a); // 18
    print(--a + 10); // 27
    print(a); // 17
    print(a-- + 9); // 26
    print(a); // 16

    a += 2;
    print(a); // 18
    a -= 2 + 0;
    print(a); // 16
    a *= 2;
    print(a); // 32
    a /= 2;
    print(a); // 16
    a %= 2;
    print(a); // 0

    print(--42);

    b--;
    print_f(b); // 2.1400
    b++;
    print_f(b); // 3.1400

    b += 2.0; 
    print_f(b); // 5.1400
    b -= 2.0;
    print_f(b); // 3.1400


    return 0;
  }
}