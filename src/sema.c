#include "tcmm.h"

#include <stdio.h>
#include <stdarg.h>
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
  const Type* ctype; // variables' type
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
    obj->base.info.ctype= type;
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
    obj->base.info.ctype= type;
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
    obj->base.info.ctype= type;
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
  FuncScp* fscp;
  size_t   vsz;
} LexScp;

typedef struct _SemaCheck {
  const char* src;
  TypeSys* tsys;
  MPool*   pool;
  LitPool* lpool;
  Scp*     scp;
  const char* err;
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

static inline void SemaCheckError( SemaCheck* p , Node* n , const char* fmt , ... ) {
  char buf[1024];
  va_list vl;
  va_start(vl,fmt);
  vsnprintf(buf,1024,fmt,vl);
  p->err = ReportErrorWithRange(p->pool,"semantic",p->src,n->dbg_start,n->dbg_end,buf);
}

/** --------------------------------------------------
 *                                                   |
 * Semantic Checking                                 |
 *                                                   |
 *   1) Type Check                                   |
 *   2) Named Variable Assignment                    |
 *   3) Type Cast                                    |
 *                                                   |
 * --------------------------------------------------*/

static const Type* SemaCheckGetRawType( SemaCheck* p , EType t ) {
  switch(t) {
    case EPT_INT: return (const Type*)(TypeSysGetInt(p->tsys));
    case EPT_DBL: return (const Type*)(TypeSysGetDbl(p->tsys));
    case EPT_CHAR:return (const Type*)(TypeSysGetChar(p->tsys));
    case EPT_BOOL:return (const Type*)(TypeSysGetBool(p->tsys));
    case EPT_VOID:return (const Type*)(TypeSysGetVoid(p->tsys));
    case ET_STR:  return (const Type*)(TypeSysGetStr(p->tsys));
    default: assert(0); return NULL;
  }
}

// NOTES: for array definition, it has its own unique type as well however it is
//        not registered during the _Check1 phase but register on demand. So when
//        we find it is an array type, we should try to get it from type system,
//        but if it failed, we should register it back to type system , ie an array
//        type will not lead to failure of type resolution
static const Type* SemaCheckGetType( SemaCheck* p , const TypeInfo* tinfo ) {
  switch(tinfo->type) {
    case ET_STRUCT: return (const Type*)(TypeSysGetStruct(p->tsys,tinfo->extra.name));
    case ET_FUNC:   return (const Type*)(TypeSysGetFunc  (p->tsys,tinfo->extra.name));
    case ET_ARR:
      {
        const Type*       at = NULL;
        const Type* sub_type = SemaCheckGetType(p,tinfo->extra.arr.t);
        if(!sub_type)
          return NULL;
        if(!(at = (const Type*)TypeSysGetArr(p->tsys,sub_type,tinfo->extra.arr.len))) {
          return (const Type*)TypeSysSetArr(p->tsys,sub_type,tinfo->extra.arr.len);
        } else {
          return at;
        }
      }
    default:
      return SemaCheckGetRawType(p,tinfo->type);
  }
}

static const char* SemaCheckGetTypeInfoStr( SemaCheck* p , const TypeInfo* tinfo ) {
  (void)p;
  return ETypeGetStr(tinfo->type);
}

// 1. Collect all the global scope type definition , including all the type
//    definition as :
//
//    1) Struct
//    2) Function
static int _Check1( SemaCheck* p , NodeFile* n ) {
  // pass 1 , register function type and struct type , but not going into
  //          each field of the struct type this make fowrad reference possible
  for( size_t i = 0 ; i < n->chunk->sz ; ++i ) {
    Node* stmt = n->chunk->stmt[i];
    switch(stmt->type) {
      case ENT_STRUCT_DEF:
        {
          NodeStructDef* sdef = (NodeStructDef*)(stmt);
          sdef->ctype         = TypeSysSetStruct(p->tsys,sdef->name);
        }
        break;
      case ENT_FUNC:
        {
          NodeFunc*      fdef = (NodeFunc*)(stmt);
          FuncType*        ft = TypeSysSetFunc(p->tsys,fdef->name);

          for( size_t i = 0 ; i < fdef->arg_sz ; ++i ) {
            NodeFuncArgDef* adef = fdef->arg + i;
            const Type*       at = SemaCheckGetType(p,&(adef->tinfo));
            if(!at) {
              SemaCheckError(p,(Node*)n,
                               "cannot find type %s in function %s's %d's argument",
                               SemaCheckGetTypeInfoStr(p,&(adef->tinfo)),
                               LitPoolId(p->lpool,fdef->name),
                               (int)(i+1));
              return -1;
            }

            adef->ctype = TypeSysFuncAddArg(p->tsys,ft,at,adef->name);
          }
          // return type
          {
            const Type* rt = SemaCheckGetType(p,&(fdef->rtype));
            if(!rt) {
              SemaCheckError(p,(Node*)n,
                               "cannot find type %s for function %s's return",
                               SemaCheckGetTypeInfoStr(p,&(fdef->rtype)),
                               LitPoolId(p->lpool,fdef->name));
              return -1;
            }

            fdef->ret_ctype = rt;
            TypeSysFuncSetRet(p->tsys,ft,rt);
          }
        }
        break;
      default:
        break;
    }
  }

  // pass 2 , go through each fields located inside of each struct definitions
  for( size_t i = 0 ; i < n->chunk->sz ; ++i ) {
    Node* stmt = n->chunk->stmt[i];
    if(stmt->type == ENT_STRUCT_DEF) {
      NodeStructDef* sdef = (NodeStructDef*)(stmt);
      StructType*      st = sdef->ctype;
      for( size_t i = 0 ; i < sdef->sz ; ++i ) {
        NodeStructDefField* field = sdef->field + i;
        const Type*             t = SemaCheckGetType(p,&(field->tinfo));
        if(!t) {
          SemaCheckError(p,(Node*)n, "cannot find type %s for struct %s's field %s",
                                     SemaCheckGetTypeInfoStr(p,&(field->tinfo)),
                                     LitPoolId(p->lpool,sdef->name),
                                     LitPoolId(p->lpool,field->name));
          return -1;
        }
        field->ctype = TypeSysAddStructField(p->tsys,st,field->name,t);
      }
    }
  }

  return 0;
}

/** Type marking, conversion **/
static const Type* SemaCheckLit    ( SemaCheck* , NodeLit* );
static const Type* SemaCheckId     ( SemaCheck* , NodeId*  );
static const Type* SemaCheckStruLit( SemaCheck* , NodeStruLit* );
static const Type* SemaCheckPrefix ( SemaCheck* , NodePrefix* );
static const Type* SemaCheckUnary  ( SemaCheck* , NodeUnary* );
static const Type* SemaCheckBinary ( SemaCheck* , NodeBinary* );
static const Type* SemaCheckTernary( SemaCheck* , NodeTernary* );
static const Type* SemaCheckExpr   ( SemaCheck* , Node* );

// Implicit type case or type resolution , work directly on AST and may add new AST
// node to make the AST becomes strict AST which means operands matched the operators
// requirements strictly. The implicit promotion rule will be applied and it is listed
// as following :
//
// 1. int -> dbl
// 2. char-> int
// 3. bool-> int
//
// In boolean context, we have following rule
//
// 1. any -> bool

static int SemaCheckTypeCast1( SemaCheck* p , const Type* lhs , const Type* rhs ,
                                                                Node**      rop ) {
  switch(lhs->tag) {
    case EPT_DBL:
      if(rhs->tag == EPT_INT) {
        Node* old = *rop;
        NodeIntToDbl* cast   = MPoolGrab(p->pool,sizeof(*cast));

        cast->base.type      = ENT_INT_TO_DBL;
        cast->base.dbg_start = old->dbg_start;
        cast->base.dbg_end   = old->dbg_end;
        cast->expr           = old;
        *rop = (Node*)cast;
        return 0;
      }
      break;
    case EPT_INT:
      if(rhs->tag == EPT_CHAR) {
        Node* old = *rop;
        NodeCharToInt* cast = MPoolGrab(p->pool,sizeof(*cast));

        cast->base.type     = ENT_CHAR_TO_INT;
        cast->base.dbg_start= old->dbg_start;
        cast->base.dbg_end  = old->dbg_end;
        cast->expr          = old;

        *rop = (Node*)cast;
        return 0;
      } else if(rhs->tag == EPT_BOOL) {
        Node* old = *rop;
        NodeBoolToInt* cast = MPoolGrab(p->pool,sizeof(*cast));

        cast->base.type     = ENT_BOOL_TO_INT;
        cast->base.dbg_start= old->dbg_start;
        cast->base.dbg_end  = old->dbg_end;
        cast->expr          = old;
        *rop = (Node*)cast;
      }
      break;
    default:
      break;
  }

  return lhs->tag == rhs->tag ? 0 : -1;
}

static const Type* SemaCheckLit( SemaCheck* p , NodeLit* e ) {
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
  return e->ctype;
}

static const Type* SemaCheckId( SemaCheck* p , NodeId* e ) {
  const Sym* sym;

  if(!(sym = FindSym(p,e->name))) {
    SemaCheckError(p,(Node*)e,"cannot find variable with name %s",LitPoolId(p->lpool,e->name));
    return NULL;
  }

  e->ref   = sym->ref;
  e->ctype = sym->info.ctype;
  return e->ctype;
}

static const Type* SemaCheckStruLit( SemaCheck* p , NodeStruLit* e ) {
  const StructType* st = TypeSysGetStruct(p->tsys,e->name);
  if(!st) {
    SemaCheckError(p,(Node*)e,"undefined type struct %s",LitPoolId(p->lpool,e->name));
    return NULL;
  }
  e->ctype = st;

  // go through each field of this assignment
  for( NodeStruLitAssign* n = e->assign ; n ; n = n->next ) {
    const FieldType* ft = TypeSysGetStructField(p->tsys,st,n->name);
    const Type*     lhs;
    const Type*     rhs;

    if(!ft) {
      SemaCheckError(p,(Node*)e,"undefined field %s in struct %s",LitPoolId(p->lpool,n->name),
          LitPoolId(p->lpool,e->name));
      return NULL;
    }

    lhs = ft->t;
    if(!(rhs = SemaCheckExpr(p,n->value)))
      return NULL;

    if(SemaCheckTypeCast1(p,lhs,rhs,&(n->value))) {
      SemaCheckError(p,(Node*)e,"field %s in struct %s type mismatch with its rhs",
                                LitPoolId(p->lpool,n->name),
                                LitPoolId(p->lpool,e->name));
      return NULL;
    }
  }
  return (const Type*)e->ctype;
}
