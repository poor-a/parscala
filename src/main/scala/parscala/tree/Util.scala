package parscala
package tree

object Util {
  def removeAnonymousPackage(t : Tree) : Tree =
    t

  def asMethod(t : SymbolTree) : Option[Either[Decl.Method, Defn.Method]] = {
	val c : Any => Option[Either[Decl.Method, Defn.Method]] = Function.const(None)
    SymbolTree.kindCata(
        defn =>
          Defn.kindCata(
              _val => None // Val
            , _var => None // Var
            , method => Some(Right(method)) // Method
            , _class => None  // Class
            , _packageObject => None // PackageObject
            , _package => None // Package
            , defn
            )
	    decl =>
		  Decl.kindCata(
			  _val => Non
        
}
