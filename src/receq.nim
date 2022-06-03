import std/[macros, genasts, sequtils, enumerate]


macro genRecEq(td: type, left, right: untyped): untyped =
  result = newStmtList()

  let typeStruct = td.getType[1][1].getTypeImpl

  var
    baseFields  : seq[NimNode]
    variantCases: NimNode
  for i, field in enumerate(typeStruct[2]):
    case field.kind
      of nnkIdentDefs:
        baseFields.add field
      of nnkRecCase:
        variantCases = field
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

  if variantCases != nil:
    let variantSwitch = variantCases[0][0]
    var cases = nnkCaseStmt.newTree(newDotExpr(left, variantSwitch))
    for c in variantCases[1..^1]:
      var branch: NimNode
      var fields: NimNode
      case c.kind
        of nnkOfBranch:
          branch = nnkOfBranch.newTree(c[0])
          fields = c[1]
        of nnkElse:
          branch = nnkElse.newTree
          fields = c[0]
        else: discard

      var comps: seq[NimNode]
      for field in (if fields.kind == nnkRecList: fields.toSeq else: @[fields]):
        comps.add: genComp(left, right, field)

      if len(comps) == 0:
        branch.add: genAst: true
      else:
        branch.add newStmtList(comps.foldl(infix(a, "and", b)))
      cases.add branch

    result.add: genAst(cases, variantSwitch, left, right):
      if left.variantSwitch == right.variantSwitch:
        cases
      else: false

  else:
    result.add: genAst: true


func `==*`*[T: ref object](left, right: T): bool =
  genRecEq(T, left, right)


func `==*`*[X: not object, T: ref X](left, right: T): bool =
  left[] == right[]


template `!=*`*[T: ref object](left, right: T): bool =
  not (left ==* right) 