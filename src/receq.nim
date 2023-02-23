import std/[macros, sequtils]


func `==*`*[T: not(ref|object)](a, b: T): bool = a == b

func `==*`*[T: ref](a, b: T): bool = a[] ==* b[]

macro impl[T: object](a, b: T): untyped =

  func gen(nodes: NimNode|seq[NimNode], a,b: NimNode): NimNode =
    result = newStmtList()
    var normalFields: seq[NimNode]
    for node in nodes:
      if node.kind == nnkRecCase:
        let discriminatorField = node[0][0]
        let discriminatorA = quote do: `a`.`discriminatorField`
        let discriminatorB = quote do: `b`.`discriminatorField`
        result.add: quote do:
          if not(`discriminatorA` ==* `discriminatorB`):
            return false

        var caseStmt = nnkCaseStmt.newTree(discriminatorA)
        for branch in node[1 .. ^1]:
          caseStmt.add:
            if branch.kind == nnkOfBranch:
              if branch[1].kind == nnkRecList:
                nnkOfBranch.newTree(branch[0], gen(branch[1], a,b))
              else:
                nnkOfBranch.newTree(branch[0], gen(branch[1..^1], a,b))
            else:
              if branch[0].kind == nnkRecList:
                nnkElse.newTree(gen(branch[0], a,b))
              else:
                nnkElse.newTree(gen(branch, a,b))
        result.add caseStmt

      else:
        normalFields.add node[0]

    if len(normalFields) > 0:
      let cond = normalFields.
        mapIt(quote do: `a`.`it` ==* `b`.`it`).
        foldl(quote do: `a` and `b`)
      result.add: quote do:
        if not `cond`: return false

  result = gen(a.getTypeImpl[2], a,b)
  result.add ident"true"
  debugEcho result.repr

func `==*`*[T: object](a, b: T): bool = impl(a, b)


template `!=*`*[T](a, b: T): bool = not(a ==* b)
