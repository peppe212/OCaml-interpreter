# Custom Language Interpreter in OCaml

---

This OCaml project implements an interpreter for a custom-defined programming language. The interpreter supports various data types, control flow structures, function definitions, and a custom dictionary data structure. A key feature of this interpreter is its ability to evaluate expressions under both **static** and **dynamic** scoping rules.

## Features

* **Abstract Syntax Tree (AST)**: Defines the structure of the language's expressions using OCaml data types.
* **Basic Data Types**: Supports integers (`int`), booleans (`bool`), and strings (`string`).
* **Arithmetic Operations**: Includes addition, subtraction, multiplication, division, and modulo for integers. Unary minus is also supported.
* **Boolean Operations**: Implements logical AND, OR, and NOT.
* **Comparison Operations**: Provides equality, less than, greater than, less than or equal to, and greater than or equal to for integers.
* **Conditional Statements**: `if-then-else` constructs for branching logic.
* **Variable Bindings**:
    * `let`: For local variable declarations.
    * `let rec`: For defining recursive functions.
* **Functions**: Supports first-class functions with application (`Apply`).
* **Custom Dictionary Type**: A mutable dictionary (or map) data structure with the following operations:
    * `Dict`: Creates a new dictionary.
    * `Get`: Retrieves a value by its key.
    * `Add`: Adds a new key-value pair. Throws an exception if the key already exists.
    * `Remove`: Removes a key-value pair.
    * `Clear`: Empties the dictionary.
    * `ApplyOver`: Applies a given function to all values within the dictionary.
* **Scope Rules**:
    * **Static Scoping (`eval`)**: Functions capture their environment at the time of their definition.
    * **Dynamic Scoping (`rt_eval`)**: Functions use the environment active at the time of their call.
* **Error Handling**: Custom exceptions for type mismatches, division by zero, and dictionary-specific errors (e.g., `KeyNotFound`, `ExistingKey`).

---

## Project Structure

The core logic of the interpreter is contained within a single OCaml file, likely `interpreter.ml` or similar. It defines:

* `ide`: Type for identifiers (strings).
* `exp`: The abstract syntax tree (AST) for expressions.
* `dictionary`: The type definition for the custom dictionary.
* `'t env`: Type for the environment, mapping identifiers to their values.
* `evT`: Expressible values (the runtime types).
* `emptyEnv`, `bind`, `lookup`: Functions for environment management.
* `typecheck`: A utility function for type checking runtime values.
* Custom exceptions (`GenericError`, `KeyNotFound`, `ExistingKey`).
* Auxiliary functions for arithmetic, boolean, and dictionary operations (e.g., `sum`, `et`, `get`, `add`).
* `eval`: The main interpreter function implementing **static scoping**.
* `rt_eval`: The main interpreter function implementing **dynamic scoping**.

---

## Getting Started

To run this project, you'll need an OCaml development environment.

1.  **Clone the repository:**
    ```bash
    git clone [https://github.com/GiuseppeMuschetta/your-repo-name.git](https://github.com/GiuseppeMuschetta/your-repo-name.git)
    cd your-repo-name
    ```
    (Remember to replace `your-repo-name` with the actual name of your repository).

2.  **Compile the OCaml code:**
    You can compile the OCaml file directly:
    ```bash
    ocamlc -o interpreter interpreter.ml
    ```
    (Assuming your main OCaml file is named `interpreter.ml`).

3.  **Run the interpreter:**
    You can then execute the compiled code:
    ```bash
    ./interpreter
    ```
    Alternatively, you can load it into the OCaml toplevel for interactive testing:
    ```bash
    ocaml
    # #use "interpreter.ml";;
    ```

---

## Usage Examples

The `TEST` section in the provided code demonstrates various functionalities. Here are some examples of how to interact with the interpreter:

* **Dictionary Operations:**
    * `e1`: Creating a dictionary.
    * `e2`: Getting a value.
    * `e3`, `e4`: Adding elements.
    * `e5`: Removing an element.
    * `e6`: Clearing a dictionary.
    * `e7`: Applying a function over dictionary values.
    * `e0`: Demonstrates an error with duplicate keys.

* **Scoping Comparison:**
    * `e8`: A classic example showcasing the difference between static and dynamic scoping.
        * When evaluated with `eval` (static scoping), `e8` will return `1`.
        * When evaluated with `rt_eval` (dynamic scoping), `e8` will return `2`.

To run these tests, you would call `eval` or `rt_eval` with the corresponding expression in the OCaml toplevel:

```ocaml
# eval e1 emptyEnv;;
- : evT = Dictionary [("one", Int 1); ("two", Int 2); ("three", Int 3); ("four", Int 4)]

# eval e2 emptyEnv;;
- : evT = Int 3

# rt_eval e8 emptyEnv;; (* Dynamic Scoping *)
- : evT = Int 2

# eval e8 emptyEnv;; (* Static Scoping *)
- : evT = Int 1
