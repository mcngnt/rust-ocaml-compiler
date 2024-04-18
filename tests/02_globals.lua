print(x)
x = "abc"
print(x)




-- { Ast.locals = [];
--   body =
--   (Ast.Seq (
--      (Ast.Seq (
--         (Ast.Seq (Ast.Nop,
--            (Ast.FunctionCall
--               ((Ast.Var (Ast.Name "print")), [(Ast.Var (Ast.Name "x"))]))
--            )),
--         (Ast.Assign ((Ast.Name "x"), (Ast.LiteralString "abc"))))),
--      (Ast.FunctionCall
--         ((Ast.Var (Ast.Name "print")), [(Ast.Var (Ast.Name "x"))]))
--      ));
--   ret = Ast.Nil }
