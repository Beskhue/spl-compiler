A compiler for the &SPL language.

# &SPL

&SPL is an acronym for Simple Programming Language, and is a C-like imperative programming language, with classes, lists, and tuples. For example, the following code produces the Fibonacci sequence.

```
#include "stdlib/std.spl"
// Output the Fibonacci sequence
main ()
{
    var t1 = 0;
    var t2 = 1;
    while(True) {
        println(t2);
        var h = t1 + t2;
        t1 = t2;
        t2 = h;
    }
}
```

# Compiling

```
> Lib.compile
> "program.spl"
```

# Pretty printing

```
> Lib.prettyPrint
> "program.spl"
```
