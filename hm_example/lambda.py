from __future__ import annotations

from typing import (
    Any as TAny,
    Callable as TCall,
    List as TList,
    Dict as TDict,
    Union as TUnion,
    Type as TType,
    TypeVar as TTypeVar,
    Generic as TGeneric,
    Tuple as TTuple,
    cast as _cast,
    Iterator as TIter,
)
import sys
from dataclasses import dataclass
import dataclasses as _dc
import typeguard
import functools
from functools import reduce as fold

_dc_attrs = {"frozen": True, "repr": False}

class ADT:
    class _MatchFail(Exception):
        pass

    _T = TTypeVar("_T")
    _U = TTypeVar("_U")

    def __iter__(self) -> TIter[TAny]:
        yield from [getattr(self, field.name) for field in _dc.fields(self)]

    def __repr__(self) -> str:
        string = f"{self.__class__.__name__}"
        if len(_dc.fields(self)) == 0:
            return string
        string += f"("
        keys = [field.name for field in _dc.fields(self)]
        for i, k in enumerate(keys):
            value = getattr(self, k)
            if isinstance(value, str):
                string += f"'{value}'"
            else:
                string += f"{value}"
            if i < len(keys) - 1:
                string +=","
        string += ")"
        return string

    def __enter__(self: _T) -> _T:
        return self

    def __exit__(self, type, value, traceback):  # type: ignore
        pass

    def __rshift__(self: _T, cls: TType[_U]) -> _U:
        if not isinstance(self, cls):
            raise ADT._MatchFail
        return self


import contextlib as ctxlib
_pm = ctxlib.suppress(ADT._MatchFail)

check_types = True
check_types = False
check_argument_types: TCall[[], bool]
if check_types:
    check_argument_types = typeguard.check_argument_types
else:
    check_argument_types = lambda: True

# --WAR-beg--  mypy issue: https://github.com/python/mypy/issues/5485
_BoxT = TTypeVar("_BoxT")
@dataclass
class Box(TGeneric[_BoxT]):
    inner: _BoxT  # pytype: disable=not-supported-yet

    @property
    def __call__(self) -> _BoxT:
        return self.inner
# --WAR-end--

Term = TUnion["Lam",
              "Var",
              "App",
              "KonstInt",
              "Unit"]

@dataclass(**_dc_attrs)
class Lam(ADT):
    name : str
    term : Term

@dataclass(**_dc_attrs)
class Var(ADT):
    name : str

@dataclass(**_dc_attrs)
class App(ADT):
    term1 : Term
    term2 : Term

@dataclass(**_dc_attrs)
class KonstInt(ADT):
    value : int

@dataclass(**_dc_attrs)
class Unit(ADT):
    pass


Typ = TUnion["TLam",
             "TBVar",
             "TVar",
             "TArr",
             "TInt",
             "TUnit"]

@dataclass(**_dc_attrs)
class TLam(ADT):
    name : str
    typ : Typ

@dataclass(**_dc_attrs)
class TBVar(ADT):
    name : str

@dataclass(**_dc_attrs)
class TVar(ADT):
    idx : int

@dataclass(**_dc_attrs)
class TArr(ADT):
    typ1 : Typ
    typ2 : Typ

@dataclass(**_dc_attrs)
class TInt(ADT):
    pass

@dataclass(**_dc_attrs)
class TUnit(ADT):
    pass

def _unhandled(*args : TAny) -> TAny:
    err = "unhandled ("
    for i, arg in enumerate(args):
        err += f"{arg}:{type(arg)}"
        if i < len(args)-1:
            err += ","
    err += ")"
    raise TypeError(f"unhandled {err}")

def substituteTy(from_ : Typ, to : Typ, typ: Typ) -> Typ:
    if from_ == typ:
        return to
    with _pm, typ >> TLam as (v,ty):
        return TLam(v, substituteTy(from_,to,ty))
    with _pm, typ >> TArr as (ty1, ty2):
        return TArr(substituteTy(from_, to, ty1),
                    substituteTy(from_, to, ty2))
    return _unhandled(from_, to, typ)

Ctx = TDict[str,Typ]

prims : Ctx
prims = {"+": TArr(TInt(), TArr(TInt(), TInt())),
         "print": TArr(TInt(), TUnit()),
         "id": TLam("a", TArr(TBVar("a"), TBVar("a")))}

trm_id : Term = Lam("x", Var("x"))
print("trm_id=", trm_id)

trm_int : Term = KonstInt(1)
print("trm_int=", trm_int)

trm_id_unit : Term = App(trm_id, Unit())
print("trm_id_unit=", trm_id_unit)

trm_higher = Lam("x", App(Var("x"), trm_int))
print("trm_higher=", trm_higher)

trm_occurs = Lam("x", App(Var("x"), Var("x")))
print("trm_occurs=", trm_occurs)


# most naive thing that does not work

def unify1(x : Typ, y : Typ) -> bool:
    return x == y

def infer1(c : Ctx, t : Term) -> Typ:
    with _pm, t >> KonstInt:
        return TInt()
    with _pm, t >> Unit:
        return TUnit()
    with _pm, t >> Var as (s,):
        return c[s]
    with _pm, t >> App as (t1,t2):
        ty1 = infer1(c,t1)
        ty2 = infer1(c,t2)
        with _pm, ty1 >> TArr as (x,y):
            if unify1(x,ty2):
                return y
            else:
                raise TypeError(f"Cannot unify ({x},{t2})")
    with _pm, t >> Lam as (v,t1):
        raise TypeError(f"no type for {v}")
    return _unhandled(c, t)

########

try:
    print(infer1(prims, trm_id)) #raises exception
except Exception as e:
    print("Exception:", e)

print(infer1(prims, trm_int))
try:
    print(infer1(prims, trm_id_unit)) #raises exception
except Exception as e:
    print("Exception:", e)

@dataclass
class State:
    store : TDict[int, Typ] = _dc.field(default_factory=dict)
    context : TDict[str, Typ] = _dc.field(default_factory=dict)
    var_count : int = 0

def newMetaVar(state : State) -> Typ:
    v = state.var_count
    state.var_count = v + 1
    return TVar(v)

def instantiate(x : Typ, state : State) -> Typ:
    check_argument_types()
    with _pm, x >> TLam as (v,ty):
        mv = newMetaVar(state)
        return instantiate(substituteTy(TBVar(v), mv, ty), state)
    return x

def infer2(trm : Term, state : State) -> Typ:
    with _pm, trm >> KonstInt:
        return TInt()
    with _pm, trm >> Unit:
        return TUnit()
    with _pm, trm >> Var as (s,):
        return instantiate(resolve(state.context[s], state), state)
    with _pm, trm >> App as (t1,t2):
        ty1 = infer2(t1, state)
        ty2 = infer2(t2, state)
        with _pm, ty1 >> TArr as (x,y):
            unify2(x,ty2,state)
            return y
        with _pm, ty1 >> TVar as (y,):
            h = newMetaVar(state)
            t = newMetaVar(state)
            unify2(ty1, TArr(h,t), state)
            unify2(h,ty2, state)
            return t
        raise TypeError(f"apply nonarrow: ({ty1},{ty2})")
    with _pm, trm >> Lam as (v, t1):
        mv = newMetaVar(state)
        state.context[v] = mv
        tbody = infer2(t1, state)
        return TArr(mv, tbody)
    return _unhandled(trm)

def unify2(ty1p: Typ, ty2p: Typ, state: State) -> None:
    check_argument_types()
    ty1 = resolve(ty1p, state)
    ty2 = resolve(ty2p, state)
    with _pm, ty1 >> TVar as (v1,):
        state.store[v1] = ty2
        return
    with _pm, ty2 >> TVar as (v2,):
        state.store[v2] = ty1
        return
    with _pm, ty1 >> TArr as (h1,t1), ty2 >> TArr as (h2,t2):
        unify2(h1,h2,state)
        unify2(t1,t2,state)
        return
    if ty1 != ty2:
        raise TypeError(f"{ty1} != {ty2}")

# get one type and returns the same type but with all tvar-references inside it
# chased out
def resolvep(seen : TList[Typ], ty : Typ, state: State) -> Typ:
    check_argument_types()
    with _pm, ty >> TVar as (v,):
        mres = state.store.get(v)
        if mres is None:
            return ty
        else:
            resp = mres
             # occurs check
            if resp in seen:
                raise TypeError(f"occurs check: ({resp},{ty})")
            res = resolvep([resp,*seen], resp, state)
            # zonking
            state.store[v] = res
            return res
    with _pm, ty >> TArr as (h,t):
        hp = resolvep(seen,h,state)
        tp = resolvep(seen,t,state)
        return TArr(hp,tp)
    with _pm, ty >> Lam as (v,t):
        return TLam(v, resolvep(seen,t,state))
    return ty

def resolve(ty : Typ, state : State) -> Typ:
    return resolvep([], ty, state)

def runInfer(trm : Term) -> Typ:
    state = State()
    return resolve(infer2(trm, state),state)


print(runInfer(trm_id))
print(runInfer(trm_int))
print(runInfer(trm_id_unit))
print(runInfer(trm_higher))
try:
    print(runInfer(trm_occurs)) # occurs check
except Exception as e:
    print("Exception:", e)
