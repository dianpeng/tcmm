#include "tcmm.h"
#include <assert.h>

PAPI void SymTableInit( SymTable* t , MPool* pool ) {
  t->sym = NULL;
  t->sz  = 0;
  t->cap = 0;
  t->pool= pool;
}

PAPI void SymTableDelete( SymTable* t ) {
  t->sym = NULL;
  t->sz  = 0;
  t->cap = 0;
  t->pool= NULL;
}

static Sym* SymTableInsert( SymTable* st , size_t objsz ) {
  Sym* ret;
  if(st->cap == st->sz) {
    size_t ncap = st->cap ? st->cap * 2 : 4;
    st->sym     = MPoolRealloc(st->pool,st->sym,st->cap*sizeof(Sym*),ncap*sizeof(Sym*));
    st->cap     = ncap;
  }
  ret = MPoolGrab(st->pool,objsz);
  st->sym[st->sz++] = ret;
  return ret;
}

static Sym* SymTableFind( SymTable* st , LitIdx idx , ESymType t ) {
  for( size_t i = 0 ; i < st->sz ; ++i ) {
    if(st->sym[i]->info.name == idx && st->sym[i]->type == t)
      return st->sym[i];
  }
  return NULL;
}

PAPI const Sym*  SymTableGetSym ( SymTable* st , LitIdx idx ) {
  for( size_t i = 0 ; i < st->sz ; ++i ) {
    if(st->sym[i]->info.name == idx)
      return st->sym[i];
  }
  return NULL;
}

PAPI const LVar* SymTableGetLVar( SymTable* st , LitIdx idx ) {
  return (const LVar*)SymTableFind(st,idx,ST_LVAR);
}

PAPI const GVar* SymTableGetGVar( SymTable* st , LitIdx idx ) {
  return (const GVar*)SymTableFind(st,idx,ST_GVAR);
}

PAPI const Arg* SymTableGetArg( SymTable* st , LitIdx idx ) {
  return (const Arg*)SymTableFind(st,idx,ST_ARG);
}

PAPI LVar* SymTableSetLVar( SymTable* st , LitIdx idx , const Type* type ) {
  assert( SymTableGetLVar(st,idx) == NULL );
  {
    LVar* obj = (LVar*)SymTableInsert(st,sizeof(LVar));
    obj->base.info.name = idx;
    obj->base.info.type = type;
    obj->base.type      = ST_LVAR;
    obj->offset    = 0;
    return obj;
  }
}

PAPI GVar* SymTableSetGVar( SymTable* st , LitIdx idx , const Type* type ) {
  assert( SymTableGetGVar(st,idx) == NULL );
  {
    GVar* obj = (GVar*)SymTableInsert(st,sizeof(GVar));
    obj->base.info.name = idx;
    obj->base.info.type = type;
    obj->base.type      = ST_GVAR;
    obj->offset         = 0;
    return obj;
  }
}

PAPI Arg* SymTableSetArg( SymTable* st , LitIdx idx , const Type* type ) {
  assert( SymTableGetArg(st,idx) == NULL );
  {
    Arg* obj = (Arg*)SymTableInsert(st,sizeof(Arg));
    obj->base.info.name = idx;
    obj->base.info.type = type;
    obj->base.type      = ST_ARG;
    obj->offset         = 0;
    return obj;
  }
}
