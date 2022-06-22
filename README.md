# RecEq

receq defines the `==*` and `!=*` operator to recursively compare 2 ref objects.<br>
It also works for not recursive and mutual recursive types.<br>
ref types that are no objects just get derefferenced and compared with `==`

### Litle Example
more can be found in the tests
```nim
type Tree = ref object
  val: int
  case isLeaf: bool
    of false: left, right: Tree
    else: discard

func val(x: int ): Tree = Tree(isLeaf: true, val: x)
func val(x: Tree): Tree = x

template tree(l: untyped, x: int, r: untyped): Tree =
  Tree(isLeaf: false, left: val l, val: x, right: val r)

assert: val(1) ==* val(1)
assert: val(1) !=* val(2)
assert: val(1) !=* tree(1, 2, 3)
assert: tree(tree(1, 2, 3), 4, tree(tree(5, 6, 7), 8, 9)) ==* tree(tree(1, 2, 3), 4, tree(tree(5, 6, 7), 8, 9))
assert: tree(tree(1, 2, 3), 4, tree(tree(5, 6, 7), 8, 9)) !=* tree(tree(1, 2, 3), 4, tree(tree(6, 6, 7), 8, 9))
assert: tree(tree(1, 2, 3), 4, tree(tree(5, 6, 7), 8, 9)) !=* tree(tree(1, 2, 3), 4, tree(5, 8, 9))
```
