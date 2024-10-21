# Sprocket Sentinel

Welcome to sprocket-sentinel, a toy compiler in c++ built for a toy language.



## Setup instructions

To setup this compiler, go to (*preferably a Linux system*) and clone this repository by:

``````bash
git clone https://github.com/buttafraz22/sprocket-sentinel/
``````



Next, compile the complete.cpp file by:

``````bash
g++ -o complete complete.cpp
``````



Run the provided sample code.afz by:

`````bash
./complete code.afz
`````



## About AFZ-lang

AFZ-lang is a simple, toy language that uses the minimalist of the grammars required to fully build and execute a compiler.

* Variable types: int, str (string), dbl (double)
* Declaration and independent assignments both allowed
* Compound expressions (those having &&, ||, !=, ==, <=, >=) are allowed as part of expressions.
* The structure of `if` is `if (expression) {}`.
* Functions declarations are similar. `function functionName(variableType id, ...) {}`.



For more information, have a look at the code.afz available. 
