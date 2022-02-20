FROM ocaml/opam

WORKDIR /home/opam/app

COPY --chown=opam . . 

RUN sudo apt install -y wget 
RUN wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key|sudo apt-key add -
RUN echo "deb http://apt.llvm.org/bullseye/ llvm-toolchain-bullseye-13 main" | sudo tee -a /etc/apt/sources.list
RUN echo "deb-src http://apt.llvm.org/bullseye/ llvm-toolchain-bullseye-13 main" | sudo tee -a /etc/apt/sources.list

RUN sudo apt update -y
RUN sudo apt install -y python3 cmake libllvm13 llvm-13 llvm-13-dev clang-13 lldb-13 lld-13
RUN opam switch create mucomp-switch ocaml-base-compiler.4.13.0
RUN opam install menhir ppx_deriving llvm dune utop