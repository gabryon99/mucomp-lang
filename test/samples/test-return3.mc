component Program provides App {
  def main() : int {
      var i : int;
      i = 1;
  
      while(i < 10){
          if(i % 2 == 0)
            return i;
        
          print(i);
          i = i + 1;  
      }
  
      return 0;
  }
}