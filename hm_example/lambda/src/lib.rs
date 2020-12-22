use std::collections::HashMap;
use std::fmt;

type Int = i32;
//-----------------------------------------------------------------------------

#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Term {
    kind: Box<TermKind>,
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Term {
    fn kind(&self) -> &TermKind {
        return &*self.kind;
    }
    pub fn new(kind: TermKind) -> Term {
        Term {
            kind: Box::new(kind),
        }
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
pub struct Type {
    kind: Box<TypeKind>,
}
impl Type {
    pub fn kind(&self) -> &TypeKind {
        return &*self.kind;
    }
    pub fn new(kind: TypeKind) -> Type {
        Type {
            kind: Box::new(kind),
        }
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
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
    match typ.kind() {
        _ if from == typ => to.clone(),
        TypeKind::TLam(v, ty) => TLam(v, subst_ty(from, to, ty)),
        TypeKind::TArr(ty1, ty2) => TArr(
            subst_ty(from, to, ty1), //
            subst_ty(from, to, ty2),
        ),
        _ => typ.clone(),
    }
}

// ----------------------------------------------------------------------------

#[derive(Debug)]
struct State {
    store: HashMap<Int, Type>,
    context: HashMap<String, Type>,
    var_count: Int,
}

impl State {
    fn new() -> State {
        State {
            store: HashMap::new(),
            context: HashMap::new(),
            var_count: 0,
        }
    }
    fn new_metavar(&mut self) -> Type {
        let v = self.var_count;
        self.var_count = v + 1;
        TVar(v)
    }

    fn instantiate(&mut self, t: &Type) -> Type {
        match t.kind() {
            TypeKind::TLam(v, ty) => {
                let mv = self.new_metavar();
                self.instantiate(&subst_ty(&TBVar(v), &mv, ty))
            }
            _ => t.clone(),
        }
    }

    fn infer2(&mut self, trm: &Term) -> Type {
        match trm.kind() {
            TermKind::KonstInt(_) => TInt(),
            TermKind::Unit => TUnit(),
            TermKind::Var(s) => {
                let ty = self.resolve(&self.context.get(s).unwrap().clone());
                self.instantiate(&ty)
            }
            TermKind::App(t1, t2) => {
                let ty1 = self.infer2(&t1);
                let ty2 = self.infer2(&t2);
                match ty1.kind() {
                    TypeKind::TArr(x, y) => {
                        self.unify2(&x, &ty2);
                        y.clone()
                    }
                    TypeKind::TVar(_) => {
                        let h = self.new_metavar();
                        let t = self.new_metavar();
                        self.unify2(&ty1, &TArr(h.clone(), t.clone()));
                        self.unify2(&h, &ty2);
                        t
                    }
                    _ => panic!("Unhandled ty1= {:?}", ty1),
                }
            }
            TermKind::Lam(v, t1) => {
                let mv = self.new_metavar();
                self.context.insert(v.to_string(), mv.clone());
                let tbody = self.infer2(&t1);
                TArr(mv, tbody)
            }
        }
    }

    fn resolve_impl(&mut self, mut seen: Vec<Type>, ty: &Type) -> Type {
        match ty.kind() {
            TypeKind::TVar(v) => {
                match self.store.get(&v) {
                    None => ty.clone(),
                    Some(resp) => {
                        // occurs check
                        if seen.contains(&resp) {
                            panic!("occurs check: ({:?},{:?})", resp, ty)
                        }
                        seen.push(resp.clone());
                        let resp = &resp.clone();
                        let res = self.resolve_impl(seen, resp);
                        // zonking
                        self.store.insert(*v, res.clone());
                        res
                    }
                }
            }
            TypeKind::TArr(h, t) => {
                let hp = self.resolve_impl(seen.clone(), &h);
                let tp = self.resolve_impl(seen, &t);
                TArr(hp, tp)
            }
            TypeKind::TLam(v, t) => TLam(&v, self.resolve_impl(seen, &t)),
            _ => ty.clone(),
        }
    }
    fn resolve(&mut self, ty: &Type) -> Type {
        self.resolve_impl(Vec::new(), ty)
    }

    fn unify2(&mut self, ty1p: &Type, ty2p: &Type) {
        let ty1 = self.resolve(ty1p);
        let ty2 = self.resolve(ty2p);
        match (ty1.kind(), ty2.kind()) {
            (TypeKind::TVar(v1), _) => {
                self.store.insert(*v1, ty2);
            }
            (_, TypeKind::TVar(v2)) => {
                self.store.insert(*v2, ty1);
            }
            (TypeKind::TArr(h1, t1), TypeKind::TArr(h2, t2)) => {
                self.unify2(&h1, &h2);
                self.unify2(&t1, &t2);
            }
            _ if ty1 == ty2 => return,
            _ => panic!("unable to unify ({:?},{:?})", ty1, ty2),
        }
    }
}

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

pub fn run_infer(term: &Term) -> Type {
    let mut state = State::new();
    let infer = state.infer2(term);
    state.resolve(&infer)
}
