print("hello", 42, true)


-- { Ast.locals = [];
--   body =
--   (Ast.Seq (Ast.Nop,
--      (Ast.FunctionCall
--         ((Ast.Var (Ast.Name "print")),
--          [(Ast.LiteralString "hello"); (Ast.Integer 42L); Ast.True;
--            (Ast.Integer 73L)]))
--      ));
--   ret = Ast.Nil }