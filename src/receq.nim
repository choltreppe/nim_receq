import std/[macros, genasts, sequtils, enumerate]


macro genRecEq(td: type, left, right: untyped): untyped =
  let
    left: NimNode = left
    right: NimNode = right

  func gen(fields: seq[NimNode]): NimNode =
    result = newStmtList()

    var
      baseFields  : seq[NimNode]
      variantCases: seq[NimNode]
    for i, field in enumerate(fields):
      case field.kind
        of nnkIdentDefs:
          baseFields.add field
        of nnkRecCase:
          variantCases.add field
        else: discard

    func genComp(left, right, field: NimNode): NimNode =
      if (if field[1].kind == nnkBracketExpr: field[1][0] else: field[1]).getTypeImpl.kind == nnkRefTy:
        genAst(left, right, field = field[0]):
          left.field ==* right.field
      else:
        genAst(left, right, field = field[0]):
          left.field == right.field

    if len(baseFields) > 0:
      let cond =
        baseFields
        .mapIt(genComp(left, right, it))
        .foldl(infix(a, "and", b))
      result.add: genAst(cond):
        if not cond: return false

    for variantCases in variantCases:
      let variantSwitch = variantCases[0][0]
      var cases = nnkCaseStmt.newTree(newDotExpr(left, variantSwitch))
      for c in variantCases[1..^1]:
        proc addCase(branch, fields: NimNode) =
          let fields =
            if fields.kind == nnkRecList: fields.toSeq
            else: @[fields]
          cases.add branch.add(gen(fields))
        case c.kind
          of nnkOfBranch:
            addCase nnkOfBranch.newTree(c[0]), c[1]
          of nnkElse:
            addCase nnkElse.newTree, c[0]
          else: discard

      result.add: genAst(cases, variantSwitch, left, right):
        if left.variantSwitch != right.variantSwitch:
          return false
        cases

  result = gen(td.getType[1][1].getTypeImpl[2].toSeq)
  result.add: genAst: true

func `==*`*[T: ref object](left, right: T): bool =
  genRecEq(T, left, right)


func `==*`*[X: not object, T: ref X](left, right: T): bool =
  left[] == right[]


template `!=*`*[T: ref object](left, right: T): bool =
  not (left ==* right) 