# albert
`Albert` is an interpreter for the `Monkey` programming language written in `Rust`

## Description
The supported data types are the following:
1. integers, e.g
```
let x = 5;
let y = -20;
```
2. the booleans `true` and `false`, e.g
```
let condition = true;
```
3. string literals enclosed in `"`, e.g
```
let name = "Bill";
```
4. function literals, e.g
```
let fib = fn(n) { if (n < 2) { n } else { fib(n-1) + fib(n-2) }};
```
5. array literals, e.g
```
let a = [1, 2, "hello", "world", [4, 2], fn(n) { n + 1}];
```

### operators
There are two kind of operators in `Albert`, infix and prefix. 
### prefix
The prefix operators are two, `!` and `-`. The former accepts integers and booleans as operands, where the later accepts only integers. 
### infix
The infix operators are of two kinds - arithmetic and relational. The arithmetic operators are the usual left associative `+,-,/` and `*`, plus the power operator `^` which is right associative. The operands of the arithmetic operators must be integers. The relational operators are `<,>,!=` and `==`. The first two (`<,>`) can be applied only to integers, where the last two (`==, !=`) can be applied to integer, boolean and string literals.

### builtins 
There is only one built in function, `len` which works for strings and arrays and returns the number of characters and elements respectively. 
