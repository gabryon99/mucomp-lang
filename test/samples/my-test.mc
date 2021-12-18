/**
 * Insertion Sort
 * Code adapted from https://www.geeksforgeeks.org/insertion-sort/
 */

interface Sorter {
    def sort(arr : int[], size : int);
}

interface AGlobal {
    var a : int;
}

component EntryPoint provides App {
  def main() : int {
      return 0;
  }
}