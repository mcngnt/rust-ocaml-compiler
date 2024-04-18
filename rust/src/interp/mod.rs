use self::{
    env::{Env, GEnv, LEnv},
    value::{Function, Value},
};
use crate::parser::ast::*;
use std::{collections::HashMap, rc::Rc};

mod env;
pub mod value;

impl Block
{
    // Interprétation d'un bloc
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast>
    {
        self.body.interp(env);
        Value::Nil
    }
}

impl Stat_
{
    // Interprétation d'une instruction
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>)
    {
        match self
        {
            Stat_::Nop => (),
            Stat_::Seq(s1, s2) => 
            {
                s1.interp(env);
                s2.interp(env);
            },
            Stat_::StatFunctionCall(fc) =>
            {
                fc.interp(env);
                ()
            },
            _ => ()
        }
    }
}

impl FunctionCall
{
    // Interprétation d'un appel de fonction
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast>
    {
        let ve = self.0.interp(env);
        let vargs : Vec<Value<'ast>> = self.1.iter().map(|e| e.interp(env)).collect();
        match ve
        {
            Value::Function(f) =>
            {
                match f
                {
                    Function::Print => 
                    {
                        let mut i = vargs.len();
                        for v in vargs.iter()
                        {
                            i -= 1;
                            print!("{}", v);
                            if i > 0
                            {
                                print!("\t");
                            }
                        }
                        print!("\n");
                        Value::Nil
                    },
                    _ => Value::Nil
                }
            },
            _ => Value::Nil
        }
    }
}


// pub enum Exp_ {
//     Nil,
//     False,
//     True,
//     Number(Number),
//     LiteralString(Rc<String>),
//     Var(Var),
//     ExpFunctionCall(FunctionCall),
//     FunctionDef(FuncBody),
//     BinOp(BinOp, Exp, Exp),
//     UnOp(UnOp, Exp),
//     Table(Vec<(Exp, Exp)>),
// }



// pub enum Value<'ast> {
//     Nil,
//     Bool(bool),
//     Number(Number),
//     String(Rc<String>),
//     Function(Function<'ast>),
//     Table(Rc<RefCell<Table<'ast>>>),
// }


impl Exp_
{
    // Interprétation d'une expression
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast>
    {
        match self
        {
            Exp_::Nil => Value::Nil,
            Exp_::Var(v) =>
            {
                match v
                {
                    Var::Name(n) =>
                    {
                        match env.globals.0.get(n)
                        {
                            Some(v) => v.clone(),
                            None => Value::Nil
                        }
                    },
                    _ => Value::Nil
                }
            },
            Exp_::False => Value::Bool(false),
            Exp_::True => Value::Bool(true),
            Exp_::Number(n) => Value::Number(n.clone()),
            Exp_::LiteralString(s) => Value::String(s.clone()),
            _ => Value::Nil
        }
    }
}

// Point d'entrée principal de l'interpréteur
pub fn run(ast: &Block)
{
    let mut globals = GEnv(HashMap::new());
    let printid = "print".to_owned();
    globals.0.insert(&printid, Value::Function(Function::Print));
    let mut env = Env
    {
        locals: Rc::new(LEnv::Nil),
        globals: &mut globals,
    };
    ast.interp(&mut env);
}
