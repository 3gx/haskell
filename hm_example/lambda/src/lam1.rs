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
    #![allow(non_snake_case)]
    fn kind(&self) -> &TermKind {
        return &*self.kind;
    }
    pub fn new(kind: TermKind) -> Term {
        Term {
            kind: Box::new(kind),
        }
    }
    pub fn Lam(s: &str, t: Term) -> Term {
        Term::new(TermKind::Lam(s.to_string(), t))
    }
    pub fn Var(s: &str) -> Term {
        Term::new(TermKind::Var(s.to_string()))
    }
    pub fn App(ta: Term, tb: Term) -> Term {
        Term::new(TermKind::App(ta, tb))
    }
    pub fn KonstInt(i: Int) -> Term {
        Term::new(TermKind::KonstInt(i))
    }
    pub fn Unit() -> Term {
        Term::new(TermKind::Unit)
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
    #[allow(non_snake_case)]
    pub fn Lam(s: &str, ta: Type) -> Type {
        Type::new(TypeKind::Lam(s.to_string(), ta))
    }
    #[allow(non_snake_case)]
    pub fn BVar(s: &str) -> Type {
        Type::new(TypeKind::BVar(s.to_string()))
    }
    #[allow(non_snake_case)]
    pub fn Var(i: Int) -> Type {
        Type::new(TypeKind::Var(i))
    }
    #[allow(non_snake_case)]
    pub fn Arr(ta: Type, tb: Type) -> Type {
        Type::new(TypeKind::Arr(ta, tb))
    }
    #[allow(non_snake_case)]
    pub fn Int() -> Type {
        Type::new(TypeKind::Int)
    }
    #[allow(non_snake_case)]
    pub fn Unit() -> Type {
        Type::new(TypeKind::Unit)
    }
}
impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum TypeKind {
    Lam(String, Type),
    BVar(String),
    Var(Int),
    Arr(Type, Type),
    Int,
    Unit,
}
impl fmt::Display for TypeKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TypeKind::Lam(s, t) => write!(f, "TLam(\"{}\", {})", s, t),
            TypeKind::BVar(s) => write!(f, "TBVar(\"{}\")", s),
            TypeKind::Var(i) => write!(f, "TVar({})", i),
            TypeKind::Arr(ta, tb) => write!(f, "TArr({}, {})", ta, tb),
            TypeKind::Int => write!(f, "TInt"),
            TypeKind::Unit => write!(f, "TUnit"),
        }
    }
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
        TypeKind::Lam(v, ty) => Type::Lam(v, subst_ty(from, to, ty)),
        TypeKind::Arr(ty1, ty2) => Type::Arr(
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
        Type::Var(v)
    }

    fn instantiate(&mut self, t: &Type) -> Type {
        match t.kind() {
            TypeKind::Lam(v, ty) => {
                let mv = self.new_metavar();
                self.instantiate(&subst_ty(&Type::BVar(v), &mv, ty))
            }
            _ => t.clone(),
        }
    }

    fn infer2(&mut self, trm: &Term) -> Type {
        match trm.kind() {
            TermKind::KonstInt(_) => Type::Int(),
            TermKind::Unit => Type::Unit(),
            TermKind::Var(s) => {
                let ty = self.resolve(&self.context.get(s).unwrap().clone());
                self.instantiate(&ty)
            }
            TermKind::App(t1, t2) => {
                let ty1 = self.infer2(&t1);
                let ty2 = self.infer2(&t2);
                match ty1.kind() {
                    TypeKind::Arr(x, y) => {
                        self.unify2(&x, &ty2);
                        y.clone()
                    }
                    TypeKind::Var(_) => {
                        let h = self.new_metavar();
                        let t = self.new_metavar();
                        self.unify2(&ty1, &Type::Arr(h.clone(), t.clone()));
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
                Type::Arr(mv, tbody)
            }
        }
    }

    fn resolve_impl(&mut self, mut seen: Vec<Type>, ty: &Type) -> Type {
        match ty.kind() {
            TypeKind::Var(v) => {
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
            TypeKind::Arr(h, t) => {
                let hp = self.resolve_impl(seen.clone(), &h);
                let tp = self.resolve_impl(seen, &t);
                Type::Arr(hp, tp)
            }
            TypeKind::Lam(v, t) => Type::Lam(&v, self.resolve_impl(seen, &t)),
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
            (TypeKind::Var(v1), _) => {
                self.store.insert(*v1, ty2);
            }
            (_, TypeKind::Var(v2)) => {
                self.store.insert(*v2, ty1);
            }
            (TypeKind::Arr(h1, t1), TypeKind::Arr(h2, t2)) => {
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
        ("+".to_string(), Type::Arr(Type::Int(), Type::Int())),
        ("print".to_string(), Type::Arr(Type::Int(), Type::Unit())),
        (
            "id".to_string(),
            Type::Lam("a", Type::Arr(Type::BVar("a"), Type::BVar("a"))),
        ),
    ]
    .iter()
    .cloned()
    .collect()
}

pub fn term_id() -> Term {
    Term::Lam("x", Term::Var("x"))
}
pub fn term_int() -> Term {
    Term::KonstInt(1)
}
pub fn term_id_unit() -> Term {
    Term::App(term_id(), Term::Unit())
}
pub fn term_higher() -> Term {
    Term::Lam("x", Term::App(Term::Var("x"), term_int()))
}
pub fn term_occurs() -> Term {
    Term::Lam("x", Term::App(Term::Var("x"), Term::Var("x")))
}

pub fn run_infer(term: &Term) -> Type {
    let mut state = State::new();
    let infer = state.infer2(term);
    state.resolve(&infer)
}
