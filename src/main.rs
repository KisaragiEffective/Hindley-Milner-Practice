use std::collections::HashMap;

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
struct Ident(String);

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
struct Def {
    lhs: Ident,
    rhs: Term,
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
enum Term {
    True,
    False,
    Not,
    Call {
        f: Box<Term>,
        ret: Box<Term>,
    },
    Lambda {
        param: Ident,
        body: Box<Term>,
    },
    Ident(Ident),
}

fn main() {
    // TODO: 項の型を調べる（未知の型は型変数として扱う）
    // TODO: 型変数の単一化 (unification)
    // TODO: 矛盾があればエラーを出力して終了、なければ推論結果を出力して終了
    println!("Hello, world!");
}

#[derive(Eq, PartialEq, Clone, Debug)]
struct TypeContext {
    map: HashMap<Ident, Type>
}

impl TypeContext {
    fn get(&self, ident: &Ident) -> Option<&Type> {
        self.map.get(ident)
    }

    fn add(&mut self, ident: Ident, tp: Type) {
        self.map.insert(ident, tp);
    }
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
enum Type {
    Bool,
    Lambda {
        arg: Box<Type>,
        ret: Box<Type>,
    },
    ForAll {
        var_ident: Ident,
        tp: Box<Type>,
    },
    TypeVar(Ident),
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
enum TypeInferError {
    Conflicted,
    UnableToUnify(String),
}

fn infer(def: Def) -> Result<TypeContext, TypeInferError> {
    let mut ctx = TypeContext {
        map: Default::default()
    };

    let Def { lhs, rhs } = def;
    match rhs {
        Term::True => {
            ctx.add(lhs, Type::Bool);
        }
        Term::False => {
            ctx.add(lhs, Type::Bool);
        }
        Term::Not => {
            panic!("AST is ill-formed");
        }
        Term::Ident(var_name) => {
            ctx.add(lhs, ctx.map.iter().find(|(t, _)| (*t).clone() == var_name).unwrap().1.clone());
        }
        Term::Call { f, ret } => {
            match *f {
                Term::Ident(_) => todo!("not yet supported (call.ident)"),
                Term::Not => {
                    match *ret {
                        Term::True => {
                            ctx.add(lhs, Type::Bool);
                        }
                        Term::False => {
                            ctx.add(lhs, Type::Bool);
                        }
                        Term::Not => panic!("AST is ill-formed (call.not.not)"),
                        Term::Call { .. } => todo!("not supported yet"),
                        Term::Ident(_) => todo!("not supported yet (call.not.ident)"),
                        other_term => {
                            return Err(TypeInferError::UnableToUnify(format!("{other_term:?} is not an function, and thus cannot be called")))
                        }
                    }
                }
                other_term => {
                    return Err(TypeInferError::UnableToUnify(format!("{other_term:?} is not an function, and thus cannot be called")))
                }
            }
        }
        Term::Lambda { param, body } => {
            match *body {
                Term::True => {
                    ctx.add(param, Type::ForAll {
                        var_ident: Ident("a".to_string()),
                        tp: Box::new(Type::Bool),
                    });
                }
                Term::False => {
                    ctx.add(param, Type::ForAll {
                        var_ident: Ident("a".to_string()),
                        tp: Box::new(Type::Bool),
                    });
                }
                Term::Not => {
                    panic!("AST is ill-formed");
                }
                Term::Call { .. } => {
                    todo!("Not supported yet");
                }
                Term::Lambda { .. } => {
                    todo!("Not supported yet");
                }
                Term::Ident(body_var) => {
                    // is `a -> a`?
                    let lambda_tp = if param == body_var {
                        Type::ForAll {
                            var_ident: Ident("a".to_string()),
                            tp: Box::new(Type::TypeVar(Ident("a".to_string())))
                        }
                    } else {
                        Type::ForAll {
                            var_ident: Ident("a".to_string()),
                            tp: Box::new(ctx.map.iter().find(|(vn, _)| (*vn).clone() == body_var).unwrap().1.clone())
                        }
                    };

                    ctx.add(param, lambda_tp);
                }
            }
        }
    };

    Ok(ctx)
}

#[cfg(test)]
mod tests {
    use crate::*;

    #[test]
    fn test_pass_1() {
        // in: x = false
        // out: x: Bool
        let def = Def {
            lhs: Ident("x".to_string()),
            rhs: Term::False,
        };

        assert_eq!(infer(def).unwrap().get(&Ident("x".to_string())).unwrap(), &Type::Bool);
    }

    #[test]
    fn test_pass_2() {
        // in: w = v -> not(v)
        let def = Def {
            lhs: Ident("w".to_string()),
            rhs: Term::Lambda {
                param: Ident("v".to_string()),
                body: Box::new(Term::Call {
                    f: Box::new(Term::Not),
                    ret: Box::new(Term::Ident(Ident("v".to_string())))
                }),
            }
        };

        // out: w: Bool -> Bool
        assert_eq!(infer(def).unwrap().get(&Ident("w".to_string())).unwrap(), &Type::Lambda {
            arg: Box::new(Type::Bool),
            ret: Box::new(Type::Bool)
        });
    }

    #[test]
    fn test_pass_3() {
        // in: y = u -> true
        let def = Def {
            lhs: Ident("y".to_string()),
            rhs: Term::Lambda {
                param: Ident("u".to_string()),
                body: Box::new(Term::True)
            }
        };

        // out: y: forall a. a -> Bool
        assert_eq!(infer(def).unwrap().get(&Ident("y".to_string())).unwrap(), &Type::ForAll {
            var_ident: Ident("a".to_string()),
            tp: Box::new(Type::Lambda {
                arg: Box::new(Type::TypeVar(Ident("a".to_string()))),
                ret: Box::new(Type::Bool)
            }),
        });
    }

    #[test]
    fn test_pass_4() {
        // in: z = v -> v
        let def = Def {
            lhs: Ident("z".to_string()),
            rhs: Term::Lambda {
                param: Ident("v".to_string()),
                body: Box::new(Term::Ident(Ident("v".to_string())))
            }
        };

        // out: z: forall a. a -> a
        assert_eq!(infer(def).unwrap().get(&Ident("y".to_string())).unwrap(), &Type::ForAll {
            var_ident: Ident("a".to_string()),
            tp: Box::new(Type::Lambda {
                arg: Box::new(Type::TypeVar(Ident("a".to_string()))),
                ret: Box::new(Type::TypeVar(Ident("a".to_string())))
            }),
        });
    }

    #[test]
    fn test_fail_1() {
        // in: x = false(true)
        let def = Def {
            lhs: Ident("x".to_string()),
            rhs: Term::Call {
                f: Box::new(Term::False),
                ret: Box::new(Term::True),
            }
        };

        // out: TypeError: `Bool` cannot be unified with `a -> b`
        assert!(infer(def).is_err());
    }
}
