package parscala

package object tree {
  type TypedDecl = Decl[Symbol, List[scalac.Type]]
  type TypedDefn = Defn[Symbol, List[scalac.Type]]
  type TypedExpr = Expr[Symbol, List[scalac.Type]]
  type TypedStatement = Statement[Symbol, List[scalac.Type]]

  type TypedExprMap = ExprMap[Symbol, List[scalac.Type]]
  type TypedDefnMap = DefnMap[Symbol, List[scalac.Type]]
  type TypedDeclMap = DeclMap[Symbol, List[scalac.Type]]

  type ExprMap[IdentInfo, SemanticInfo] = Map[SLabel, tree.Expr[IdentInfo, SemanticInfo]]
  type DefnMap[IdentInfo, SemanticInfo] = Map[DLabel, tree.Defn[IdentInfo, SemanticInfo]]
  type DeclMap[IdentInfo, SemanticInfo] = Map[DLabel, tree.Decl[IdentInfo, SemanticInfo]]
  type SymbolTable = Map[Symbol, DLabel]
}
