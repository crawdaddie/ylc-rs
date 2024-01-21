build_aux_lib:
  clang -shared -o libffi.so clibs/libffi.c -fPIC -g -O0 


build_and_link FILE:
  cargo run -- {{FILE}}
  @just build_aux_lib
  clang -o exe object -L./ -lffi
  

