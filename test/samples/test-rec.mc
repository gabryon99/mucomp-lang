component EntryPoint provides App {

  def fib(n: int): int {
    if (n == 0) {
      return 0;
    }

    if (n == 1) {
      return 1;
    }

    return fib(n - 1) + fib(n - 2);
  }

  def sum(a: int, b: int): int {
    if (a > 0) {
      return sum(a - 1, b + 1);
    }
    return b;
  }

  def fact(n: int) : int {
    if (n == 0) {
      return 1;
    }
    else {
      return n * fact(n - 1);
    }
  }

  def test(n: int): int {

    while (n > 0) {
      if (n == 4) {
        return 4;
      }
      else {
        n = n - 1;
      }
    }

    return 5;
  }

  def is_even(x: int) : bool {
    if ((x % 2) == 0) {
      return true;
    }
    else {
      return false;
    }
  }

  def is_odd(x: int) : bool {
    return !(is_even(x));
  }

  /**
  * Exponentiation by squaring.
  * From: https://en.wikipedia.org/wiki/Exponentiation_by_squaring
  */
  def exp_by_squaring(x: int, n: int) : int {

    if (n < 0) {
      return exp_by_squaring(1 / x, -n);
    }
    else {
      if (n == 0) {
        return 1;
      }
      else {
        if (n == 1) {
          return x;
        }
        else {
          if (is_even(n)) {
            return exp_by_squaring(x * x, n / 2);
          }
          else {
            return x * exp_by_squaring(x * x, (n - 1) / 2);
          }
        }
      }
    }
  }

  def get_number(ch: char) : int {
    putc(ch); putc(':'); putc(' ');
    return getint();
  }

  def main() : int {
    
    var base: int;
    var exp: int;

    base = get_number('b');
    print(base);
    exp = get_number('e');

    print(exp_by_squaring(base, exp));

    return 0;
  }
}