# Statically typed Lisp with Interpreter, Compiler and VM

I read [Peter Norvig's Lisp Interpreter Tutorial](http://norvig.com/lispy.html)
but halfway through I wondered how hard it would be to add a type system without knowing any theory.
While at it I also added a compiler and virtual machine which evaluates the compiled bytecode.

Because this was just a little fun learning experience the implementations are really minimal.
For example the virtual machine only works with integer and bool values. In the future I might add
a heap and (simple mark and sweep) garbage collection to allow for more complex data types.


## Example

### Code

```lisp 
;; nth factorial number
(defn fac (params n)
  (if (> n 1)
      (* n (fac (- n 1)))
      1))

(fac 7)
```

### Typed Abstract Syntax Tree

```lisp
(do::Num
  (defn::Nil
    :fac::Sym
    (params::Nil
      :n::Num)
    (if::Num
      (<::Bool
        :n::Num
        2)
      1
      (*::Num
        :n::Num
        (fac::Num
          (-::Num
            :n::Num
            1)))))
  (fac::Num
    7))
```  

### Bytecode

```assembly
0:   JMP   26
2:   STORE 0
4:   LOAD  0
6:   PUSH  2
8:   LT    
9:   JIF   23
11:  LOAD  0
13:  LOAD  0
15:  PUSH  1
17:  SUB   
18:  CALL  2
20:  MUL   
21:  JMP   25
23:  PUSH  1
25:  END   
26:  PUSH  7
28:  CALL  2
30:  HALT  
```