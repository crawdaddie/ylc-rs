build_aux_lib:
  clang -shared -o libffi.so clibs/libffi.c -Werror -Wall -Wextra -fPIC 


build FILE:
  cargo run -- {{FILE}}
  echo $(shell basename -s . $(FILE))

