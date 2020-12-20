
type Int = i32;
type DynTerm = Box<Term>;
enum Term {
    Lam(String, DynTerm),
    Var(String),
    App(DynTerm, DynTerm),
    KonstInt(Int),
    Unit,
}

type DynTyp = Box<Typ>;
enum Typ {
    TLam(String, DynTyp),
    TBVar(String),
    TVar(Int),
    TArr(DynTyp, DynTyp),
    TInt,
    TUnit,
}
