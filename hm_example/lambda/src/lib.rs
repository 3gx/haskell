type Int = i32;
type DynTerm = Box<Term>;
#[derive(PartialEq, Debug)]
pub enum Term {
    Lam(String, DynTerm),
    Var(String),
    App(DynTerm, DynTerm),
    KonstInt(Int),
    Unit,
}

type DynTyp = Box<Typ>;
#[derive(PartialEq, Debug, Clone)]
pub enum Typ {
    TLam(String, DynTyp),
    TBVar(String),
    TVar(Int),
    TArr(DynTyp, DynTyp),
    TInt,
    TUnit,
}

fn mk_new<T>(val: T) -> Box<T> {
    Box::new(val)
}

pub fn subst_ty(from: Typ, to: Typ, typ: Typ) -> Typ {
    use Typ::{TArr, TLam};
    if from == typ {
        to
    } else {
        match typ {
            TLam(v, ty) => TLam(v, mk_new(subst_ty(from, to, *ty.clone()))),
            TArr(ty1, ty2) => TArr(
                mk_new(subst_ty(from.clone(), to.clone(), *ty1)),
                mk_new(subst_ty(from.clone(), to.clone(), *ty2)),
            ),
            _ => typ,
        }
    }
}
