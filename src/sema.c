#include "tcmm.h"
#include <assert.h>

/* -------------------------------------------------------
 * Symbol
 * ------------------------------------------------------*/
typedef enum _ESymType {
  ST_ARG,
  ST_GVAR,
  ST_LVAR,
  ST_UNKNOWN
} ESymType;

typedef struct _SymInfo {
  LitIdx      name; // variable name
  const Type* type; // variables' type
} SymInfo;

typedef struct _Sym {
  SymInfo      info;
  ESymType     type;
	IdRef         ref;
} Sym;

typedef struct _LVar {
  Sym      base;    // symbol of the local varaibles
  size_t offset;    // offset from the frame registers
} LVar;

typedef struct _GVar {
  Sym      base;    // symbol of the global variables
  size_t offset;    // offset from the global variable region
} GVar;

typedef struct _Arg {
  Sym      base;
  size_t offset;
} Arg;

typedef struct _SymTable {
  Sym**   sym;
  size_t   sz;
  size_t  cap;
  MPool*  pool;
} SymTable;

static void SymTableInit( SymTable* t , MPool* pool ) {
  t->sym = NULL;
  t->sz  = 0;
  t->cap = 0;
  t->pool= pool;
}

static void SymTableDelete( SymTable* t ) {
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

static const Sym*  SymTableGetSym ( SymTable* st , LitIdx idx ) {
  for( size_t i = 0 ; i < st->sz ; ++i ) {
    if(st->sym[i]->info.name == idx)
      return st->sym[i];
  }
  return NULL;
}

static const LVar* SymTableGetLVar( SymTable* st , LitIdx idx ) {
  const Sym* sym = SymTableGetSym(st,idx);
  assert(sym->type == ST_LVAR);
  return (const LVar*)sym;
}

static const GVar* SymTableGetGVar( SymTable* st , LitIdx idx ) {
  const Sym* sym = SymTableGetSym(st,idx);
  assert(sym->type == ST_GVAR);
  return (const GVar*)sym;
}

static const Arg* SymTableGetArg( SymTable* st , LitIdx idx ) {
  const Sym* sym = SymTableGetSym(st,idx);
  assert(sym->type == ST_ARG);
  return (const Arg*)sym;
}

static LVar* SymTableSetLVar( SymTable* st , LitIdx idx , const Type* type ) {
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

static GVar* SymTableSetGVar( SymTable* st , LitIdx idx , const Type* type ) {
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

static Arg* SymTableSetArg( SymTable* st , LitIdx idx , const Type* type ) {
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

/** ------------------------------------------------
 * Scope
 * ------------------------------------------------*/

struct _Scp;

typedef enum _EScpType {
  SCPT_GLOBAL,
  SCPT_FUNC  ,
  SCPT_LEXICAL,
  SCPT_UNKNOWN
} EScpType;

// Scope , used during parsing phase
typedef struct _Scp {
  struct _Scp* prev;  // previous scope
  EScpType     type;  // types of the scope
  SymTable      stb;
} Scp;

typedef struct _GlbScp {
  Scp base;
} GlbScp;

typedef struct _FuncScp {
  Scp             base;
  const FuncType* type;
  size_t          max_vxz; // maximum stack size
} FuncScp;

typedef struct _LexScp {
  Scp      base;
  FuncScp* fscp;        // top level function scope , no closure support so relative simple
  size_t   vsz;         // all variable accumulated size that is nested up to now
} LexScp;

typedef struct _SemaCheck {
  TypeSys* tsys;
  MPool*   pool;
  LitPool* lpool;
  Scp*     scp;
} SemaCheck;

static LexScp* LexScpEnter( SemaCheck* p , LexScp* lscp ) {
  Scp* pscp = p->scp;

  lscp->base.prev = pscp;
  lscp->base.type = SCPT_LEXICAL;
  SymTableInit(&(lscp->base.stb),p->pool);

  if(pscp->type == SCPT_LEXICAL) {
    LexScp* pp = (LexScp*)pscp;
    // accumulated variable size
    lscp->vsz  = pp->vsz + pp->base.stb.sz;
    lscp->fscp = pp->fscp;
  } else {
    assert(pscp->type == SCPT_FUNC);
    {
      FuncScp* fscp = (FuncScp*)pscp;
      lscp->vsz     = 0;
      lscp->fscp    = fscp;
    }
  }

  // update the current scope context
  p->scp = (Scp*)lscp;
  return lscp;
}

static FuncScp* FuncScpEnter( SemaCheck* p , FuncScp* scp , const FuncType* ft ) {
  assert(p->scp->type == SCPT_GLOBAL);
  {
    GlbScp* pscp = (GlbScp*)(p->scp);
    scp->base.prev = (Scp*)pscp;
    scp->base.type = SCPT_FUNC;
    SymTableInit(&(scp->base.stb),p->pool);
    scp->type = ft;
    scp->max_vxz = 0;

    p->scp = (Scp*)scp;
    return scp;
  }
}

static Scp* ScpLeave( SemaCheck* p ) {
  Scp* scp = p->scp;

  if(scp->type == SCPT_LEXICAL) {
    LexScp* lscp = (LexScp*)(scp);

    size_t max_vsz = lscp->vsz + lscp->base.stb.sz;
    if(max_vsz > lscp->fscp->max_vxz)
      lscp->fscp->max_vxz = max_vsz;
  }
  SymTableDelete(&(scp->stb));
  p->scp = scp->prev;
  return p->scp;
}

static inline FuncScp* CurFScp( SemaCheck* p ) {
  assert( p->scp && p->scp->type == SCPT_FUNC );
  return (FuncScp*)(p->scp);
}

static inline LexScp* CurLScp ( SemaCheck* p ) {
  assert( p->scp && p->scp->type == SCPT_LEXICAL );
  return (LexScp*)(p->scp);
}

static inline GlbScp* CurGScp ( SemaCheck* p ) {
  assert( p->scp && p->scp->type == SCPT_GLOBAL );
  return (GlbScp*)(p->scp);
}

static inline FuncScp* FScp( SemaCheck* p ) {
  if(p->scp->type == SCPT_FUNC)
    return (FuncScp*)p->scp;
  if(p->scp->type == SCPT_LEXICAL)
    return ((LexScp*)(p->scp))->fscp;

  return NULL;
}

// resolve a symbol name based on a literal index
static inline const Sym* FindSym( SemaCheck* p , LitIdx name ) {
  Scp* cscp = p->scp;
  while(cscp) {
    const Sym* ret = SymTableGetSym(&(cscp->stb),name);
    if(ret) return ret;
    cscp = cscp->prev;
  }
  return NULL;
}

static inline LVar* DefineLVar( SemaCheck* p , LitIdx name , const Type* t ) {
  assert(p->scp->type == SCPT_LEXICAL);
  {
    LVar* lvar;
    LexScp* lscp = (LexScp*)p->scp;

    if(SymTableGetLVar(&(p->scp->stb),name))
      return NULL;
    lvar = SymTableSetLVar(&(p->scp->stb),name,t);

    lvar->base.ref.ref_type = EIRT_LOCAL;
    lvar->base.ref.d.slots  = lscp->vsz + p->scp->stb.sz - 1;

    return lvar;
  }
}

static inline GVar* DefineGVar( SemaCheck* p , LitIdx name , const Type* t ) {
  assert(p->scp->type == SCPT_GLOBAL);
  {
    GVar* gvar;
    if(SymTableGetGVar(&(p->scp->stb),name))
      return NULL;
    gvar = SymTableSetGVar(&(p->scp->stb),name,t);
    gvar->base.ref.ref_type = EIRT_GLOBAL;
    gvar->base.ref.d.slots  = p->scp->stb.sz - 1;
    return gvar;
  }
}

static inline Arg* DefineArg( SemaCheck* p , LitIdx name , const Type* t ) {
  assert(p->scp->type == SCPT_FUNC);
  {
    Arg* arg;
    if(SymTableGetArg(&(p->scp->stb),name))
      return NULL;
    arg = SymTableSetArg(&(p->scp->stb),name,t);
    arg->base.ref.ref_type = EIRT_ARG;
    arg->base.ref.d.slots  = p->scp->stb.sz - 1;
    return arg;
  }
}

/** --------------------------------------------------
 *                                                   |
 * Semantic Checking --> for simple type checking    |
 *                                                   |
 * --------------------------------------------------*/

static int CheckLit    ( SemaCheck* , const NodeLit* );
static int CheckId     ( SemaCheck* , const NodeId*  );
static int CheckStruLit( SemaCheck* , const NodeStruLit* );
static int CheckPrefix ( SemaCheck* , const NodePrefix* );
static int CheckUnary  ( SemaCheck* , const NodeUnary* );
static int CheckBinary ( SemaCheck* , const NodeBinary* );
static int CheckTernary( SemaCheck* , const NodeTernary* );


static int CheckLit( SemaCheck* p , const NodeLit* e ) {
  const Lit* l = LitPoolIndex(p->lpool,e->lit);
  switch(l->type) {
    case ELT_INT:
      e->ctype = (const Type*)TypeSysGetInt(p->tsys);
      break;
    case ELT_DBL:
      e->ctype = (const Type*)TypeSysGetDbl(p->tsys);
      break;
    case ELT_STR:
      e->ctype = (const Type*)TypeSysGetStr(p->tsys);
      break;
    case ELT_CHAR:
      e->ctype = (const Type*)TypeSysGetChar(p->tsys);
      break;
    case ELT_TRUE:
    case ELT_FALSE:
      e->ctype = (const Type*)TypeSysGetBool(p->tsys);
      break;
    default:
      assert(0);
      break;
  }
  return 0;
}

static int CheckId( SemaCheck* p , const NodeId* e ) {
}

















