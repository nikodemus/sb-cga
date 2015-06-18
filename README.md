# SB-CGA

SB-CGA is a computer graphics algebra library for Common Lisp.

Despite the prefix it is actually portable - but optimizations that
make it fast (using SIMD instructions) are currently implemented for
SBCL/x86-64 only.

There is no fundamental reason why these could not be implemented for
other implementations as well, though.

SB-CGA is not under active development currently, but neither is it
quite abandoned. Patches will get looked at, though you may need to
kick Nikodemus once or twice to get his attention.

There's a manual: http://nikodemus.github.io/sb-cga/

There's a TODO in TODO.org.

There's a pending re-implementation of the root solvers in new-roots.lisp.
