use std::collections::HashMap;
use serde::Deserialize;

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
    Call(Box<Term>, Box<Term>),
    Lambda(Ident, Box<Term>),
    Ident(Ident),
}

fn main() {
    // TODO: 項の型を調べる（未知の型は型変数として扱う）
    // TODO: 型変数の単一化 (unification)
    // TODO: 矛盾があればエラーを出力して終了、なければ推論結果を出力して終了
    println!("Hello, world!");
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
struct TypeContext {
    map: HashMap<Ident, Type>
}

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

fn infer(def: Def) -> Result<TypeContext, ()> {
    todo!()
}

#[test]
fn test_pass_1() {
    // in: x = false
    // out: x: Bool
    let def = Def {
        lhs: Ident("x".to_string()),
        rhs: Term::False,
    };

    assert_eq!(infer(def).unwrap().map.get(&Ident("x".to_string())), Type::Bool);
}

#[test]
fn test_pass_2() {
    // in: w = v -> not(v)
    let def = Def {
        lhs: Ident("w".to_string()),
        rhs: Term::Lambda(
            Ident("v".to_string()),
            Box::new(Term::Call(
                Box::new(Term::Not),
                Box::new(Term::Ident(Ident("v".to_string())))
            ))
        )
    };

    // out: w: Bool -> Bool
    assert_eq!(infer(def).unwrap().map.get(&Ident("w".to_string())), Type::Lambda {
        arg: Box::new(Type::Bool),
        ret: Box::new(Type::Bool)
    });
}

#[test]
fn test_pass_3() {
    // in: y = u -> true
    let def = Def {
        lhs: Ident("y".to_string()),
        rhs: Term::Lambda(
            Ident("u".to_string()),
            Box::new(Term::True)
        )
    };

    // out: y: forall a. a -> Bool
    assert_eq!(infer(def).unwrap().map.get(&Ident("y".to_string())), Type::ForAll {
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
        rhs: Term::Lambda(
            Ident("v".to_string()),
            Box::new(Term::Ident(Ident("v".to_string())))
        )
    };

    // out: z: forall a. a -> a
    assert_eq!(infer(def).unwrap().map.get(&Ident("y".to_string())), Type::ForAll {
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
        rhs: Term::Call(
            Box::new(Term::False),
            Box::new(Term::True),
        )
    };

    // out: TypeError: `Bool` cannot be unified with `a -> b`
    assert!(infer(def).is_err());
}
