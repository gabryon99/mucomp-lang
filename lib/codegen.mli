exception Codegen_error of string

val to_llvm_module : Ast.typed_compilation_unit -> Llvm.llmodule 