import java

predicate stats(string stat, int value) {
	( stat = "CU" and value = count(CompilationUnit cu | cu.fromSource() ) ) or
	( stat = "LOC" and value = sum(CompilationUnit cu || cu.getNumberOfLinesOfCode()) ) or
	( stat = "Method" and value = count(Method m | exists(Block b | m.getBody() = b)) ) or
	( stat = "MethodWithCast" and value = count(Callable m | exists(CastExpr ce | ce.getEnclosingCallable() = m)) ) or
	( stat = "Stmt" and value = count(Stmt s) ) or
	( stat = "Expr" and value = count(Expr e) ) or
	( stat = "Cast" and value = count(CastExpr e) )
}

from string stat, int value
where stats(stat, value)
select stat, value