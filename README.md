# albert
`Albert` is an interpreter for the `Monkey` programming language and written in `Rust`. The source code is based on the 
code described in the following two books:
* [Writing an Interpreter in Go](https://interpreterbook.com/)
* [Writing a Compiler in Go](https://compilerbook.com/)

The original source code is in `Go`.

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

If a script file is not give, both of these commands will load the `REPL`. 

There is actually a third backend implemented, `vmd` that dumps the bytecode of a given script. 

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
| Hash Tables |        :X:         |         :X:        |
| Builtins    |        :X:         |         :X:        |
| Power `^` op| :heavy_check_mark: | :heavy_check_mark: |
| Macros      |        :X:         |         :X:        |
  

