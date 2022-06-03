import unittest

import receq


test "basic":
  type
    ListKind = enum lkCons, lkNil
    List = ref object
      case kind: ListKind
        of lkCons:
          init: List
          last: int
        of lkNil: discard

  func `<>`(init: List, last: int): List =
    List(kind: lkCons, init: init, last: last)

  func `<>`(init, last: int): List =
    List(kind: lkCons, init: List(kind: lkCons, init: List(kind: lkNil), last: init), last: last)

  check: (1<>2<>3) ==* (1<>2<>3)
  check: (1<>2<>3) !=* (4<>5<>6)
  check: (1<>2<>3) !=* (1<>2<>3<>4)
  check: (1<>2<>3<>4) !=* (1<>2<>3)



test "generic":
  type
    ListKind = enum lkCons, lkNil
    List[T] = ref object
      case kind: ListKind
        of lkCons:
          init: List[T]
          last: T
        of lkNil: discard

  func `<>`[T](init: List[T], last: T): List[T] =
    List[T](kind: lkCons, init: init, last: last)

  func `<>`[T](init, last: T): List[T] =
    List[T](kind: lkCons, init: List[T](kind: lkCons, init: List[T](kind: lkNil), last: init), last: last)

  check: (1<>2<>3) ==* (1<>2<>3)
  check: (1<>2<>3) !=* (1<>2)

  check: ("a"<>"b"<>"c") ==* ("a"<>"b"<>"c")
  check: ("a"<>"b"<>"c") !=* ("b"<>"a"<>"c")



test "with fields not part of variants":
  type Tree = ref object
    val: int
    case isLeaf: bool
      of false: left, right: Tree
      else: discard

  func val(x: int ): Tree = Tree(isLeaf: true, val: x)
  func val(x: Tree): Tree = x

  template tree(l: untyped, x: int, r: untyped): Tree =
    Tree(isLeaf: false, left: val l, val: x, right: val r)

  check: val(1) ==* val(1)
  check: val(1) !=* val(2)
  check: val(1) !=* tree(1, 2, 3)
  check: tree(tree(1, 2, 3), 4, tree(tree(5, 6, 7), 8, 9)) ==* tree(tree(1, 2, 3), 4, tree(tree(5, 6, 7), 8, 9))
  check: tree(tree(1, 2, 3), 4, tree(tree(5, 6, 7), 8, 9)) !=* tree(tree(1, 2, 3), 4, tree(tree(6, 6, 7), 8, 9))
  check: tree(tree(1, 2, 3), 4, tree(tree(5, 6, 7), 8, 9)) !=* tree(tree(1, 2, 3), 4, tree(5, 8, 9))



test "without variants (not recursive so kinda useless)":
  type Test = ref object
    a, b: string

  check: Test(a: "x", b: "y") ==* Test(a: "x", b: "y")
  check: Test(a: "x", b: "y") !=* Test(a: "", b: "")