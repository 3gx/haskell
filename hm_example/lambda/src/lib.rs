use std::ops::{Deref, DerefMut};
use std::fmt;

type Int = i32;
//-----------------------------------------------------------------------------


#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Term(Box<TermKind>);

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Term {
    pub fn new(kind: TermKind) -> Term {
        Term(Box::new(kind))
    }
}

impl Deref for Term {
    type Target = TermKind;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}
impl DerefMut for Term {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut *self.0
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

impl fmt::Display for TermKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TermKind::Lam(s, t) => write!(f, "Lam(\"{}\", {})", s, t),
            TermKind::Var(s) => write!(f, "Var(\"{}\")", s),
            TermKind::App(ta, tb) => write!(f, "App({},{})", ta, tb),
            TermKind::KonstInt(i) => write!(f, "KonstInt({})", i),
            TermKind::Unit => write!(f, "Unit"),
        }
    }
}


#[allow(non_snake_case)]
pub fn Lam(s:&str, t: Term) -> Term {
    Term::new(TermKind::Lam(s.to_string(), t))
}
#[allow(non_snake_case)]
pub fn Var(s:&str) -> Term {
    Term::new(TermKind::Var(s.to_string()))
}
#[allow(non_snake_case)]
pub fn App(ta: Term, tb : Term) -> Term {
    Term::new(TermKind::App(ta, tb))
}
#[allow(non_snake_case)]
pub fn KonstInt(i: Int) -> Term {
    Term::new(TermKind::KonstInt(i))
}
#[allow(non_snake_case)]
pub fn Unit() -> Term {
    Term::new(TermKind::Unit)
}

//-----------------------------------------------------------------------------


#[derive(Eq, PartialEq, Debug, Clone)]
pub struct Type(Box<TypeKind>);
impl Type {
    pub fn new(kind: TypeKind) -> Type {
        Type(Box::new(kind))
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
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
impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeKind::TLam(s, t) => write!(f, "TLam(\"{}\", {})", s, t),
            TypeKind::TBVar(s) => write!(f, "TBVar(\"{}\")", s),
            TypeKind::TVar(i) => write!(f, "Var({})", i),
            TypeKind::TArr(ta, tb) => write!(f, "TArr({},{})", ta, tb),
            TypeKind::TInt => write!(f, "TInt"),
            TypeKind::TUnit => write!(f, "TUnit"),
        }
    }
}
#[allow(non_snake_case)]
pub fn TLam(s: &str, ta : Type) -> Type {
    Type::new(TypeKind::TLam(s.to_string(), ta))
}
#[allow(non_snake_case)]
pub fn TBvar(s: &str) -> Type {
    Type::new(TypeKind::TBVar(s.to_string()))
}
#[allow(non_snake_case)]
pub fn Tvar(i: Int) -> Type {
    Type::new(TypeKind::TVar(i))
}
#[allow(non_snake_case)]
pub fn TArr(ta : Type, tb : Type) -> Type {
    Type::new(TypeKind::TArr(ta, tb))
}
#[allow(non_snake_case)]
pub fn TInt() -> Type {
    Type::new(TypeKind::TInt)
}
#[allow(non_snake_case)]
pub fn TUnit() -> Type {
    Type::new(TypeKind::TUnit)
}

//-----------------------------------------------------------------------------

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
    [
        (
            "+".to_string(),
            TArr(TInt(), TInt()),
        ),
        (
            "print".to_string(),
            TArr(TInt(), TUnit()),
        ),
        (
            "id".to_string(),
            TLam("a", TArr(TBvar("a"), TBvar("a"))),
        ),
    ]
    .iter()
    .cloned()
    .collect()
}

pub fn term_id() -> Term {
    Lam("x", Var("x"))
}
