use std::ops::{Deref, DerefMut};

type Int = i32;

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Term(Box<TermKind>);

impl Term {
    pub fn new(kind: TermKind) -> Term {
        Term(Box::new(kind))
    }
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum TermKind {
    Lam(String, Term),
    Var(String),
    App(Term, Term),
    KonstInt(Int),
    Unit,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Type(Box<TypeKind>);

impl Type {
    pub fn new(kind: TypeKind) -> Type {
        Type(Box::new(kind))
    }
}

impl Deref for Type {
    type Target = TypeKind;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl DerefMut for Type {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.0
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum TypeKind {
    TLam(String, Type),
    TBVar(String),
    TVar(Int),
    TArr(Type, Type),
    TInt,
    TUnit,
}

/*
from: https://stackoverflow.com/questions/28392008/more-concise-hashmap-initialization
macro_rules! hashmap {
    ($( $key: expr => $val: expr ),*) => {{
         let mut map = ::std::collections::HashMap::new();
         $( map.insert($key, $val); )*
         map
    }}
}
// let counts = hashmap!['A' => 0, 'C' => 0, 'G' => 0, 'T' => 0];.
*/

pub fn subst_ty(from: &Type, to: &Type, typ: &Type) -> Type {
    use TypeKind::{TArr, TLam};
    match &**typ {
        _ if from == typ => to.clone(),
        TLam(v, ty) => Type::new(TLam(
            v.clone(), //
            subst_ty(from, to, ty),
        )),
        TArr(ty1, ty2) => Type::new(TArr(
            subst_ty(from, to, ty1), //
            subst_ty(from, to, ty2),
        )),
        _ => typ.clone(),
    }
}

use std::collections::HashMap;
type Ctx = HashMap<String, Type>;

pub fn prims() -> Ctx {
    use TypeKind::{TArr, TBVar, TInt, TLam, TUnit};
    [
        (
            "+".to_string(),
            Type::new(TArr(Type::new(TInt), Type::new(TInt))),
        ),
        (
            "print".to_string(),
            Type::new(TArr(Type::new(TInt), Type::new(TUnit))),
        ),
        (
            "id".to_string(),
            Type::new(TLam(
                "a".to_string(),
                Type::new(TArr(
                    Type::new(TBVar("a".to_string())),
                    Type::new(TBVar("a".to_string())),
                )),
            )),
        ),
    ]
    .iter()
    .cloned()
    .collect()
}
