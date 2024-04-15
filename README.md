# ylc
simple language for learning purposes, originally conceived as an interface for https://github.com/crawdaddie/yalce audio library

compiler built in Rust with LLVM as a backend via [inkwell-rs](https://github.com/TheDan64/inkwell) (self-hosting way out of scope for now)

# compile .ylc file
`cargo run -- examples/pattern_match.ylc`   
currently the compiler only supports building object files and linking to create an executable   
(named 'exe' by default for now)   

# repl
`cargo run -- -i`

# tests
cargo test

# features
- [x] arrays
- [x] recursion 
- [x] currying
- [x] first-class functions:    
    ```javascript
    let x = fn (a, b, c) {
      if a == b {
        2 * a
      } else {
        b + c
      }
    }
    ```
- [ ] pattern-matching (almost ready):   
    ```javascript
    let x = 2
    
    
    let y = match x
      | 1 -> 1 
      | 2 -> 2 
      | 3 -> 3 
      | _ -> 4 
     
    
    printf("%d\n", y)
    ```
- [x] static, implicit typing
- [x] monomorphic function variants:  
    the following separate function applications: `f(1)` & `f(1.0)` will result in 
    the compilation of two separate functions `f_Int` & `f_Num`
- [ ] import statements for modules
- [x] external function declarations:   
    ```javascript
    let printf = fn (fmt: str, ...): int 
    let y = 1
    printf("%d\n", y)
    ```   
    a function declaration without a body is an external function which is available after linking the object files generated by the compiler

# REFERENCES
- https://craftinginterpreters.com/a-bytecode-virtual-machine.html
- http://lucacardelli.name/Papers/BasicTypechecking.pdf
- https://github.com/k-mrm/type-inference
- https://cs3110.github.io/textbook/chapters/interp/inference.html

