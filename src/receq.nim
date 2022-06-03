import std/[macros, genasts, strutils, sequtils, enumerate]


macro genRecEq(td: type, left, right: untyped): untyped =
  result = newStmtList()

  func baseType(td: NimNode): string =
    case td.kind
      of nnkBracketExpr: td[0].strVal
      else             : td.strVal

  let
    typeDef    = td.getTypeImpl[1].baseType
    typeStruct = td.getType[1][1].getTypeImpl

  var
    baseFields  : seq[NimNode]
    variantCases: NimNode
  for i, field in enumerate(typeStruct[2]):
    case field.kind
      of nnkIdentDefs:
        baseFields.add field[0]
      of nnkRecCase:
        variantCases = field
      else: discard

  if len(baseFields) > 0:
    let cond =
      baseFields
      .mapIt(genAst(it, left, right) do:
          left.it == right.it)
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
      for field in fields:
        comps.add:
          if cmpIgnoreStyle(field[1].baseType, typeDef) == 0:
            genAst(left, right, field = field[0]):
              left.field ==* right.field
          else:
            genAst(left, right, field = field[0]):
              left.field == right.field

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


template `!=*`*[T: ref object](left, right: T): bool =
  not (left ==* right) 