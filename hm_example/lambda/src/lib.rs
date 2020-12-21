use std::fmt;
use std::ops::{Deref, DerefMut};

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
            TermKind::App(ta, tb) => write!(f, "App({}, {})", ta, tb),
            TermKind::KonstInt(i) => write!(f, "KonstInt({})", i),
            TermKind::Unit => write!(f, "Unit"),
        }
    }
}

#[allow(non_snake_case)]
pub fn Lam(s: &str, t: Term) -> Term {
    Term::new(TermKind::Lam(s.to_string(), t))
}
#[allow(non_snake_case)]
pub fn Var(s: &str) -> Term {
    Term::new(TermKind::Var(s.to_string()))
}
#[allow(non_snake_case)]
pub fn App(ta: Term, tb: Term) -> Term {
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
            TypeKind::TArr(ta, tb) => write!(f, "TArr({}, {})", ta, tb),
            TypeKind::TInt => write!(f, "TInt"),
            TypeKind::TUnit => write!(f, "TUnit"),
        }
    }
}
#[allow(non_snake_case)]
pub fn TLam(s: &str, ta: Type) -> Type {
    Type::new(TypeKind::TLam(s.to_string(), ta))
}
#[allow(non_snake_case)]
pub fn TBVar(s: &str) -> Type {
    Type::new(TypeKind::TBVar(s.to_string()))
}
#[allow(non_snake_case)]
pub fn TVar(i: Int) -> Type {
    Type::new(TypeKind::TVar(i))
}
#[allow(non_snake_case)]
pub fn TArr(ta: Type, tb: Type) -> Type {
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
    match &**typ {
        _ if from == typ => to.clone(),
        TypeKind::TLam(v, ty) => TLam(v, subst_ty(from, to, ty)),
        TypeKind::TArr(ty1, ty2) => TArr(
            subst_ty(from, to, ty1), //
            subst_ty(from, to, ty2),
        ),
        _ => typ.clone(),
    }
}

use std::collections::HashMap;
type Ctx = HashMap<String, Type>;

pub fn prims() -> Ctx {
    [
        ("+".to_string(), TArr(TInt(), TInt())),
        ("print".to_string(), TArr(TInt(), TUnit())),
        ("id".to_string(), TLam("a", TArr(TBVar("a"), TBVar("a")))),
    ]
    .iter()
    .cloned()
    .collect()
}

pub fn term_id() -> Term {
    Lam("x", Var("x"))
}
pub fn term_int() -> Term {
    KonstInt(1)
}
pub fn term_id_unit() -> Term {
    App(term_id(), Unit())
}
pub fn term_higher() -> Term {
    Lam("x", App(Var("x"), term_int()))
}
pub fn term_occurs() -> Term {
    Lam("x", App(Var("x"), Var("x")))
}

// ----------------------------------------------------------------------------

#[derive(Debug)]
struct State {
    store : HashMap<Int, Type>,
    context : HashMap<String, Type>,
    var_count : Int,
}

impl State {
    fn new_metavar(&mut self) -> Type {
        let v = self.var_count;
        self.var_count = v + 1;
        TVar(v)
    }

    fn instantiate(&mut self, t: &Type) -> Type {
        match &**t {
            TypeKind::TLam(v, ty) => {
                let mv = self.new_metavar();
                self.instantiate(&subst_ty(&TBVar(v), &mv, ty))
            },
            _ => t.clone()
        }
    }

    fn infer2(&mut self, trm: &Term) -> Type {
        match &**trm {
            TermKind::KonstInt(_) => TInt(),
            TermKind::Unit => TUnit(),
            TermKind::Var(s) => {
                let ty = self.resolve(&self.context.get(s).unwrap().clone());
                self.instantiate(&ty)
            }
            TermKind::App(t1,t2) => {
                let ty1 = self.infer2(&t1);
                let ty2 = self.infer2(&t2);
                match *ty1.0 {
                    TypeKind::TArr(x,y) => {
                        self.unify2(&x,&ty2);
                        y
                    }
                    TypeKind::TVar(_) => {
                        let h = self.new_metavar();
                        let t = self.new_metavar();
                        self.unify2(&ty1, &TArr(h.clone(),t.clone()));
                        self.unify2(&h, &ty2);
                        t
                    }
                    _ => panic!("Unhandled ty1= {:?}", ty1)
                }
            }
            TermKind::Lam(v,t1) => {
                let mv = self.new_metavar();
                self.context.insert(v.to_string(),mv.clone());
                let tbody = self.infer2(&t1);
                TArr(mv, tbody)
            }
        }
    }

    fn resolve(&mut self, _t: &Type) -> Type {
        unimplemented!()
    }

    fn unify2(&mut self, _t1: &Type, _t2: &Type) {
        unimplemented!()
    }

}
