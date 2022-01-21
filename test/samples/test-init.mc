component Application provides App {
  
  var unknown_int: int;
  var a_global_int: int = 30;
  var another_global_int: int = a_global_int + 12;

  def print_bool(a: bool): void {
      
      if (a) {
        print(1);
      }
      else {
        print(0);
      }

      return;
  }

  def main() : int {
    
    unknown_int = 17;

    var a_int: int = 42;
    var a_float: float = 3.14;
    var a_chr: char = 'a';
    var a_false_bool: bool = false;
    var a_true_bool: bool = true;

    print(a_int);
    print(another_global_int);
    print(unknown_int);
    print_f(a_float);
    putc(a_chr); putc('\n');
    print_bool(a_false_bool);
    print_bool(a_true_bool);

    return 0;
  }
}