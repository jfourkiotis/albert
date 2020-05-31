# albert
`Albert` is an interpreter for the `Monkey` programming language and written in `Rust`. The source code is based on the 
code described in the following two books:
* [Writing an Interpreter in Go](https://interpreterbook.com/)
* [Writing a Compiler in Go](https://compilerbook.com/)

The original source code was written in `Go`.

### usage
There are two backends implemented. The first one, is a classic _tree-walking_ interpreter that can be invoked by 
using the command:
```shell script
repl -b tw path/to/scriptfile
```

The second backend is a bytecode compiler that can be invoked by using the command:
```shell script
repl -b vm path/to/scriptfile
```

If a script file is not given, both of these commands will load the `REPL`. 

There is actually a third backend implemented, `vmd` that dumps the bytecode of a given script. For example, consider 
the following script:

```
let fib = fn(n) {
    if (n < 2) { 
        n 
    } else {
        fib(n-1) + fib(n-2)
    }
};
fib(33);
```

Assuming the above script is in a file named `fib.mnk`, we can dump bytecode information by invoking the command:
```shell script
repl -b vmd fib.mnk
```
and get the following output:
```
[CONSTANTS]
0000: 2
0001: 1
0002: 2
0003: function(0)
0004: 33
[STRINGS]
[INSTRUCTIONS]
[   0] CLOSURE 3 0 
[   4] SET_GLOBAL 0 
[   7] GET_GLOBAL 0 
[  10] CONSTANT 4 
[  13] CALL 1 
[  15] POP 
[FUNCTIONS]
#0000
[   0] GET_LOCAL 0 
[   2] CONSTANT 0 
[   5] LESS_THAN 
[   6] JUMP_IF_FALSE 14 
[   9] GET_LOCAL 0 
[  11] JUMP 33 
[  14] CURRENT_CLOSURE 
[  15] GET_LOCAL 0 
[  17] CONSTANT 1 
[  20] SUB 
[  21] CALL 1 
[  23] CURRENT_CLOSURE 
[  24] GET_LOCAL 0 
[  26] CONSTANT 2 
[  29] SUB 
[  30] CALL 1 
[  32] ADD 
[  33] RETVAL
```

### features
Not all features of the original `Monkey` language are implemented. Those implemented for the two supported 
backends are shown in the following table:

| feature     |        `tw`        |         `vm`       |
| ----------- | ------------------ | ------------------ |
| Numbers     | :heavy_check_mark: | :heavy_check_mark: |
| Strings     | :heavy_check_mark: | :heavy_check_mark: |
| Arrays      | :heavy_check_mark: | :heavy_check_mark: |
| Functions   | :heavy_check_mark: | :heavy_check_mark: |
| Closures    | :heavy_check_mark: | :heavy_check_mark: |
| Hash Tables | :heavy_multiplication_x: |  :heavy_multiplication_x: |
| Builtins    | :heavy_multiplication_x: |  :heavy_multiplication_x: |
| Power `^` op|    :heavy_check_mark:    |     :heavy_check_mark:    |
| Macros      | :heavy_multiplication_x: |  :heavy_multiplication_x: |
  
### features scheduled for future versions
| feature     |        version       |   status |
| ----------- | -------------------- | -------- |
| Better error messages     | `v0.2` | `master` |
| Faster execution (`tw`)   | `v0.2` | `master` |
| double-precision numbers instead of integers  | `v0.2` || 
| `print` builtin           | `v0.2` || 
| support for `while` loops | `v0.2` || 
| a module system           | `v0.2` ||
| macros                    | `v0.3` ||
