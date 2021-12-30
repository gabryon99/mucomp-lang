<p align='center'>
    <img src='./assets/comu-logo.png' width='400'/>
</p>

## μcomp-lang

μcomp-lang is a didactic language implemented for *Language, Compilers and Iterpreters* course at **UniPi**.

### Example

The snippet below shows a simple program written in μcomp.

```

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

    def main() : int {
        print(fib(10)); // prints 10
        return 0;
    }
}

```

#### Trivia

The mascotte's name is "*Comu*" = "🐄" + "μ" (like: "muuuh") (I know, it's a bad pun).
