# rcc Compiler

## Overview
rcc is a lightweight C compiler supporting essential language features, including arithmetic and logical operations, primitive types, one-dimensional arrays, pointers, loops, functions, and structs. It provides a simplified subset of C while maintaining expressive power for low-level programming.

## Supported Features
### **1. Arithmetic and Logical Operations**
- Basic arithmetic: `+`, `-`, `*`, `/`, `%`
- Some Bitwise operations: `~`
- Logical operations: `&&`, `||`, `!`

### **2. Primitive Types**
- `int`, `char`, `void`, `short`, `long`
- Pointers to primitives

### **3. One-Dimensional Arrays**
- Declaration and usage of fixed-size arrays
- Example:
  ```c
  int arr[5];
  arr[0] = 10;
  ```

### **4. Pointers**
- Basic pointer operations (`*`, `&`)
- No Pointer arithmetic
- Pointer dereferencing

### **5. Control Flow Constructs**
- `for` loops
- `while` loops
- `if-else` statements
- Example:
  ```c
  int sum(int n) {
      int s = 0;
      for (int i = 0; i < n; i++) {
          s += i;
      }
      return s;
  }
  ```

### **6. Functions**
- Functions with up to **4 arguments**
- Supported return types: `int`, `char`, `void`, `long`, `short`
- Argument types: primitives, primitive pointers
- Example:
  ```c
  int add(int a, int b) {
      return a + b;
  }
  ```

### **7. Structs**
- Structs can contain:
  - Primitive types
  - Pointers
  - Other structs
  - Self-referencing pointers
  - one dimensional arrays
- Example:
  ```c
  typedef struct Node {
      int value;
      struct Node* next;
  } Node;

  void set_value(Node* n, int v) {
      n->value = v;
  }
  ```

## Example Code
```c
typedef struct Point {
    int x;
    int y;
} Point;

int main() {
    Point p;
    p.x = 10;
    p.y = 20;

    Point* ptr = &p;
    ptr->x = 30;

    return ptr->y;
}
```

## Installation
To build **rcc**, use:
```sh
cargo build
```

## Usage
Now every file compiles to test.s
To compile a C file:
```sh
./rcc input.c 
```
To assemble and run:
```sh
gcc test.s -o output
./output
```
## Testing
since every file compiles to same name .s file, run test in one thread:
```sh
cargo test -- --test-threads=1
```
## Limitations
- No support for floating-point types (`float`, `double`, `bool`)
- No support for multidimensional arrays
- No dynamic memory allocation (`malloc`, `free`)
- No `switch` statements
- No function pointers yet
- Might have edge cases not considered

## License
MIT License

