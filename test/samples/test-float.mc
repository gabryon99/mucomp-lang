component EntryPoint provides App {
  
    def isEven(n: int): bool {
        return (n % 2) == 0;
    }

    def fact(n: int): int {
        if (n == 0 || n == 1) {
            return 1;
        }
        return n * fact(n - 1);
    }

    def euler_number(iter: int): float {

        var i: int;
        var euler: float = 1.0;

        for (i = 1; i < iter; i++) {
            euler = euler + (1.0 / (fint(fact(i))));
        }

        return euler;
    }

    def pi(iter: int): float {

        // See: https://en.wikipedia.org/wiki/Leibniz_formula_for_π

        var i : int;

        var pi: float;
        var temp: float = 1.0;

        for (i = 0; i < iter; i++) {
            if (isEven(i)) {
                pi += (1.0 / temp);
            }
            else {
                pi -= (1.0 / temp);
            }

            temp += 2.0;
        }

        return pi * 4.0;
    }
  
  def main() : int {
    
    print_f(pi(1000));
    print_f(euler_number(12));

    return 0;
  }
}