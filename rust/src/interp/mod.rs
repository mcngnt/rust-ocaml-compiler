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
        let nenv = &mut Env{globals : env.globals,  locals : env.locals.extend(&self.locals, std::iter::empty())};
        self.body.interp(nenv);
        self.ret.interp(nenv)
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
            Stat_::Assign(v, e) =>
            {
                match v
                {
                    Var::Name(n) => 
                    {
                        let ve = e.interp(env);
                        env.set(n, ve)
                    },
                    Var::IndexTable(e1, e2) =>
                    {
                        let t = e1.interp(env).as_table();
                        let k = e2.interp(env).as_table_key();
                        let ve = e.interp(env);
                        t.borrow_mut().insert(k, ve);
                    }
                }
            },
            Stat_::If(e, si, se) =>
            {
                let ve = e.interp(env);
                if ve.as_bool()
                {
                    si.interp(env);
                }
                else
                {
                    se.interp(env);
                }
            },
            Stat_::WhileDoEnd(e,s) =>
            {
                let mut ve = e.interp(env);
                while ve.as_bool()
                {
                    s.interp(env);
                    ve = e.interp(env);
                }
            }
        }
    }
}

impl FunctionCall
{
    // Interprétation d'un appel de fonction
    fn interp<'ast, 'genv>(&'ast self, env: &mut Env<'ast, 'genv>) -> Value<'ast>
    {
        let f = self.0.interp(env).as_function();
        let vargs : Vec<Value<'ast>> = self.1.iter().map(|e| e.interp(env)).collect();
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
            Function::Closure(nl, lenv, b) =>
            {
                b.interp(&mut Env{globals : env.globals,  locals : lenv.extend(nl, vargs.into_iter())})
            }
        }
    }
}

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
                    Var::Name(n) => env.lookup(n),
                    Var::IndexTable(e1, e2) =>
                    {
                        // let k = e2.interp(env).as_table_key();
                        // let t_clone = e1.interp(env).as_table().clone();
                        // let mut t = t_clone.borrow_mut();
                        // let t_ref = &mut *t;
                        // match t_ref.get(&k)
                        // {
                        //     Some(v) => v.clone(),
                        //     None => Value::Nil
                        // }
                        let k = e2.interp(env).as_table_key();
                        let t = e1.interp(env).as_table();
                        let res = match t.borrow().get(&k).cloned()
                                  {
                                      Some(v) => v,
                                      None => Value::Nil,
                                  };
                        res
                    }
                }
            },
            Exp_::False => Value::Bool(false),
            Exp_::True => Value::Bool(true),
            Exp_::Number(n) => Value::Number(n.clone()),
            Exp_::LiteralString(s) => Value::String(s.clone()),
            Exp_::UnOp(op, e) =>
            {
                let ve = e.interp(env);
                match op
                {
                    UnOp::Not => Value::Bool(!(ve.as_bool())),
                    UnOp::UnaryMinus => ve.neg()
                }
            },
            Exp_::BinOp(op, e1, e2) =>
            {
                let ve1 = e1.interp(env);
                match op
                {
                    BinOp::LogicalAnd =>
                    {
                        match ve1.as_bool()
                        {
                            false => ve1,
                            true => e2.interp(env)
                        }
                    },
                    BinOp::LogicalOr =>
                    {
                        match ve1.as_bool()
                        {
                            true => ve1,
                            false => e2.interp(env)
                        }
                    },
                    _ =>
                    {
                        let ve2 = e2.interp(env);
                        match op
                        {
                            BinOp::Addition => ve1.add(ve2),
                            BinOp::Subtraction => ve1.sub(ve2),
                            BinOp::Multiplication => ve1.mul(ve2),
                            BinOp::Equality => Value::Bool(ve1.eq(&ve2)),
                            BinOp::Inequality => Value::Bool(!ve1.eq(&ve2)),
                            BinOp::Less => Value::Bool(ve1.lt(ve2)),
                            BinOp::LessEq => Value::Bool(ve1.le(ve2)),
                            BinOp::Greater => Value::Bool(ve2.lt(ve1)),
                            BinOp::GreaterEq => Value::Bool(ve2.le(ve1)),
                            _ => Value::Nil
                        }
                    }
                }
            }
            Exp_::FunctionDef(fb) => Value::Function(Function::Closure(&fb.0, Rc::clone(&env.locals), &fb.1)),
            Exp_::ExpFunctionCall(fc) => fc.interp(env),
            Exp_::Table(l) =>
            {
                let mut t = HashMap::new();
                l.iter().for_each(   |(e1, e2)| {t.insert(e1.interp(env).as_table_key(), e2.interp(env));}   );
                Value::Table(Rc::new(std::cell::RefCell::new(t)))
            },
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
