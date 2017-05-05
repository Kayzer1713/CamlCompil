link to the Github project: https://github.com/Kayzer1713/CamlCompil
# Synopsis
The goal of this project is to develop a **compilator** that's able to translate basic **C programs** to **Java bytecode**.

# Installation
To install the project you have to **download** the .zip file or **clone** the directory which is public.
This done you will need to launch a **make** command in the dowloaded directory.
After that there's no more to do! The project is installed.

# Work
## Part 1
Implementing type verification and code generation for expressions.
### Difficulties:
Prepare the environement to provide the best workflow.
Some type error when Caml was inferencing the *tp_expr* function.

## Part 2

### Exercise 6
Easy peasy lemon squeezy. No comment to add, the code compile everything is OK.

  Implemented:
- tp_stmt()

### Exercise 7
Some Difficulties to implement tp_fdefn because of dealing with temporary environments and argList checking

  Implemented:
* listVarFun()
* tp_fdefn()
* updateEnv()

### Exercise 8
No difficulty here just the logical continuation of the question 7

  Implemented:
* tp_prog()

### Exercise 9
Creating the function that translate expr into a list of instr in order to launch some basic expr.
In order to achieve that the function take an expr and return the list of instr in the right order like the call stack should have been.
The IfThenElse works in a Goto way. If the test return false the expr treated is the one of the vFalse var and if the test return a true then the vEnd expr is used.
For the functions the params of the function are inserted in the stack and the function is invoked on those params.

  Implemented:
* gen_expr()

### Exercise 10
Same as exercise 9 but for stmts.

  Implemented:
* gen_stmt()

### Exercise 11

  Implemented:
* gen_stmt()

### Exercise 12


### Exercise 13

### Exercise 14
# Still no finished...
