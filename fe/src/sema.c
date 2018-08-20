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

typedef struct _SemaChk {
  const char* src;
  TypeSys* tsys;
  MPool*   pool;
  LitPool* lpool;
  Scp*     scp;
  const char* err;
} SemaChk;

static LexScp* LexScpEnter( SemaChk* p , LexScp* lscp ) {
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

static FuncScp* FuncScpEnter( SemaChk* p , FuncScp* scp , const FuncType* ft ) {
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

static Scp* ScpLeave( SemaChk* p ) {
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

static inline FuncScp* CurFScp( SemaChk* p ) {
  assert( p->scp && p->scp->type == SCPT_FUNC );
  return (FuncScp*)(p->scp);
}

static inline LexScp* CurLScp ( SemaChk* p ) {
  assert( p->scp && p->scp->type == SCPT_LEXICAL );
  return (LexScp*)(p->scp);
}

static inline GlbScp* CurGScp ( SemaChk* p ) {
  assert( p->scp && p->scp->type == SCPT_GLOBAL );
  return (GlbScp*)(p->scp);
}

static inline FuncScp* FScp( SemaChk* p ) {
  if(p->scp->type == SCPT_FUNC)
    return (FuncScp*)p->scp;
  if(p->scp->type == SCPT_LEXICAL)
    return ((LexScp*)(p->scp))->fscp;

  return NULL;
}

// resolve a symbol name based on a literal index
static inline const Sym* FindSym( SemaChk* p , LitIdx name ) {
  Scp* cscp = p->scp;
  while(cscp) {
    const Sym* ret = SymTableGetSym(&(cscp->stb),name);
    if(ret) return ret;
    cscp = cscp->prev;
  }
  return NULL;
}

static inline LVar* DefineLVar( SemaChk* p , LitIdx name , const Type* t ) {
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

static inline GVar* DefineGVar( SemaChk* p , LitIdx name , const Type* t ) {
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

static inline Arg* DefineArg( SemaChk* p , LitIdx name , const Type* t ) {
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

static inline void SemaChkError( SemaChk* p , Node* n , const char* fmt , ... ) {
  char buf[1024];
  va_list vl;
  va_start(vl,fmt);
  vsnprintf(buf,1024,fmt,vl);
  p->err = ReportErrorWithRange(p->pool,"semantic",p->src,n->dbg_start,n->dbg_end,buf);
}

/** --------------------------------------------------
 *                                                   |
 * Semantic Chking                                 |
 *                                                   |
 *   1) Type Chk                                   |
 *   2) Named Variable Assignment                    |
 *   3) Type Cast                                    |
 *                                                   |
 * --------------------------------------------------*/

static const Type* SemaChkGetRawType( SemaChk* p , EType t ) {
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
//        not registered during the _Chk1 phase but register on demand. So when
//        we find it is an array type, we should try to get it from type system,
//        but if it failed, we should register it back to type system , ie an array
//        type will not lead to failure of type resolution
static const Type* SemaChkGetType( SemaChk* p , const TypeInfo* tinfo ) {
  switch(tinfo->type) {
    case ET_STRUCT: return (const Type*)(TypeSysGetStruct(p->tsys,tinfo->extra.name));
    case ET_FUNC:   return (const Type*)(TypeSysGetFunc  (p->tsys,tinfo->extra.name));
    case ET_ARR:
      {
        const Type*       at = NULL;
        const Type* sub_type = SemaChkGetType(p,tinfo->extra.arr.t);
        if(!sub_type)
          return NULL;
        if(!(at = (const Type*)TypeSysGetArr(p->tsys,sub_type,tinfo->extra.arr.len))) {
          return (const Type*)TypeSysSetArr(p->tsys,sub_type,tinfo->extra.arr.len);
        } else {
          return at;
        }
      }
    default:
      return SemaChkGetRawType(p,tinfo->type);
  }
}

static const char* SemaChkGetTypeInfoStr( SemaChk* p , const TypeInfo* tinfo ) {
  (void)p;
  return ETypeGetStr(tinfo->type);
}

// 1. Collect all the global scope type definition , including all the type
//    definition as :
//
//    1) Struct
//    2) Function
static int _Chk1( SemaChk* p , NodeFile* n ) {
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
            const Type*       at = SemaChkGetType(p,&(adef->tinfo));
            if(!at) {
              SemaChkError(p,(Node*)n,
                               "cannot find type %s in function %s's %d's argument",
                               SemaChkGetTypeInfoStr(p,&(adef->tinfo)),
                               LitPoolId(p->lpool,fdef->name),
                               (int)(i+1));
              return -1;
            }

            adef->ctype = TypeSysFuncAddArg(p->tsys,ft,at,adef->name);
          }
          // return type
          {
            const Type* rt = SemaChkGetType(p,&(fdef->rtype));
            if(!rt) {
              SemaChkError(p,(Node*)n,
                               "cannot find type %s for function %s's return",
                               SemaChkGetTypeInfoStr(p,&(fdef->rtype)),
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
        const Type*             t = SemaChkGetType(p,&(field->tinfo));
        if(!t) {
          SemaChkError(p,(Node*)n, "cannot find type %s for struct %s's field %s",
                                     SemaChkGetTypeInfoStr(p,&(field->tinfo)),
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

// Expression
static const Type* SemaChkLit    ( SemaChk* , NodeLit* );
static const Type* SemaChkId     ( SemaChk* , NodeId*  );
static const Type* SemaChkStruLit( SemaChk* , NodeStruLit* );
static const Type* SemaChkDot    ( SemaChk* , NodeDot* );
static const Type* SemaChkIndex  ( SemaChk* , NodeIndex* );
static const Type* SemaChkCall   ( SemaChk* , NodeCall*  );
static const Type* SemaChkUnary  ( SemaChk* , NodeUnary* );
static const Type* SemaChkBinary ( SemaChk* , NodeBinary* );
static const Type* SemaChkTernary( SemaChk* , NodeTernary* );
static const Type* SemaChkExpr   ( SemaChk* , Node* );

// Implicit type case or type resolution , work directly on AST and may add new AST
// node to make the AST becomes strict AST which means operands matched the operators
// requirements strictly. The implicit promotion rule will be applied and it is listed
// as following :
//
//  int -> dbl
//
// In boolean context, we have following rule
//
//  any -> bool

static int SemaChkTypeCast1( SemaChk* p , const Type* lhs , const Type* rhs ,
                                                                Node**      rop ) {
  if(lhs->tag == EPT_DBL && rhs->tag == EPT_INT) {
    Node* old = *rop;
    NodeIntToDbl* cast   = MPoolGrab(p->pool,sizeof(*cast));

    cast->base.type      = ENT_INT_TO_DBL;
    cast->base.dbg_start = old->dbg_start;
    cast->base.dbg_end   = old->dbg_end;
    cast->expr           = old;
    *rop = (Node*)cast;
    return 0;
  }
  return lhs->tag == rhs->tag ? 0 : -1;
}

static Node* SemaChkToBool( SemaChk* p , Node* old ) {
  NodeToBool* tb = MPoolGrab(p->pool,sizeof(*tb));
  tb->base.dbg_start = old->dbg_start;
  tb->base.dbg_end   = old->dbg_end;
  tb->base.type      = ENT_TO_BOOL;
  tb->expr           = old;
  return (Node*)tb;
}

static const Type* SemaChkLit( SemaChk* p , NodeLit* e ) {
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

static const Type* SemaChkId( SemaChk* p , NodeId* e ) {
  const Sym* sym;

  if(!(sym = FindSym(p,e->name))) {
    SemaChkError(p,(Node*)e,"cannot find variable with name %s",LitPoolId(p->lpool,e->name));
    return NULL;
  }

  e->ref   = sym->ref;
  e->ctype = sym->info.ctype;
  return e->ctype;
}

static const Type* SemaChkStruLit( SemaChk* p , NodeStruLit* e ) {
  const StructType* st = TypeSysGetStruct(p->tsys,e->name);
  if(!st) {
    SemaChkError(p,(Node*)e,"undefined type struct %s",LitPoolId(p->lpool,e->name));
    return NULL;
  }
  e->ctype = st;

  // go through each field of this assignment
  for( NodeStruLitAssign* n = e->assign ; n ; n = n->next ) {
    const FieldType* ft = TypeSysGetStructField(p->tsys,st,n->name);
    const Type*     lhs;
    const Type*     rhs;

    if(!ft) {
      SemaChkError(p,(Node*)e,"undefined field %s in struct %s",LitPoolId(p->lpool,n->name),
          LitPoolId(p->lpool,e->name));
      return NULL;
    }

    lhs = ft->t;
    if(!(rhs = SemaChkExpr(p,n->value)))
      return NULL;

    if(SemaChkTypeCast1(p,lhs,rhs,&(n->value))) {
      SemaChkError(p,(Node*)e,"field %s in struct %s type mismatch with its rhs",
                                LitPoolId(p->lpool,n->name),
                                LitPoolId(p->lpool,e->name));
      return NULL;
    }
  }
  return (const Type*)e->ctype;
}

static const Type* SemaChkDot( SemaChk* p , NodeDot* e ) {
  const Type* lhs_type = SemaChkExpr(p,e->lhs);
  const StructType* st = NULL;
  const FieldType*  ft = NULL;

  if(!lhs_type) return NULL;

  // a dot operator can only be appiled on type with *struct*
  if(lhs_type->tag != ET_STRUCT) {
    SemaChkError(p,(Node*)e,"dot operator can only apply on type struct");
    return NULL;
  }

  // find the field name
  st = (const StructType*)lhs_type;
  ft = TypeSysGetStructField(p->tsys,st,e->name);
  if(!ft) {
    SemaChkError(p,(Node*)e,"cannot find field %s from struct %s",LitPoolId(p->lpool,e->name),
                                                                    LitPoolId(p->lpool,st->name));
    return NULL;
  }

  e->ctype = ft;
  return ft->t;
}

static const Type* SemaChkIndex( SemaChk* p , NodeIndex* e ) {
  const Type* lhs_type = NULL;
  const Type* rhs_type = NULL;

  if(!(rhs_type = SemaChkExpr(p,e->rhs)))
    return NULL;

  if(SemaChkTypeCast1(p,(const Type*)(TypeSysGetInt(p->tsys)),rhs_type,&(e->rhs))) {
    SemaChkError(p,(Node*)e,"the index expression's type is not int or cannot be casted to int");
    return NULL;
  }

  if(!(lhs_type = SemaChkExpr(p,e->lhs)))
    return NULL;

  if(lhs_type->tag != ET_ARR && lhs_type->tag != ET_STR) {
    SemaChkError(p,(Node*)e,"index operator can only apply on array or string");
    return NULL;
  }
  if(lhs_type->tag == ET_STR) {
    e->ctype = (const Type*)(TypeSysGetChar(p->tsys));
    return e->ctype;
  } else {
    const ArrType* at = (const ArrType*)(lhs_type);
    e->ctype = at->type;
    return e->ctype;
  }
}

static const Type* SemaChkCall( SemaChk* p , NodeCall* e ) {
  const Type* lhs_type = SemaChkExpr(p,e->lhs);
  if(!lhs_type)
    return NULL;

  if(lhs_type->tag != ET_FUNC) {
    SemaChkError(p,(Node*)e,"function call on a none function type");
    return NULL;
  }

  {
    const FuncType* ft = (const FuncType*)lhs_type;
    if(ft->arg_size != e->arg.sz) {
      SemaChkError(p,(Node*)e,"function %s's call argument size mismatch",LitPoolIndex(p->lpool,ft->name));
      return NULL;
    }

    for( size_t i = 0 ; i < e->arg.sz ; ++i ) {
      const Type* atype = SemaChkExpr(p,e->arg.args[i]);
      const Type* etype = ft->arg[i].type;
      if(!atype) return NULL;
      if(SemaChkTypeCast1(p,etype,atype,e->arg.args+i)) {
        SemaChkError(p,(Node*)e,"function %s's call's %d's argument type mismatch",
                                  LitPoolIndex(p->lpool,ft->name),(int)i);
        return NULL;
      }
    }

    return (e->ctype = ft->ret);
  }
}

static const Type* SemaChkUnary( SemaChk* p , NodeUnary* e ) {
  const Type* opr_type = SemaChkExpr(p,e->opr);
  if(!opr_type) return NULL;
  return (e->ctype = opr_type);
}

static int BinaryPromotion( SemaChk* p , const Type* lt , const Type* rt , NodeBinary* e ) {
#ifndef NDEBU
  int r = 0;
#endif // NDEBUG
  if(lt->tag == EPT_INT && rt->tag == EPT_DBL) {
#ifndef NDEBUG
    r =
#endif // NDEBUG
      SemaChkTypeCast1(p,rt,lt,&(e->lhs));
  } else if(lt->tag == EPT_DBL && rt->tag == EPT_INT) {
#ifndef NDEBUG
    r =
#endif // NDEBUG
      SemaChkTypeCast1(p,lt,rt,&(e->rhs));
  } else {
    return -1;
  }
  assert(r);
  return 0;
}

static const Type* SemaChkBinary( SemaChk* p , NodeBinary* e ) {
  const Type* lhs_type;
  const Type* rhs_type;

  if((!(lhs_type = SemaChkExpr(p,e->lhs))) || (!(rhs_type = SemaChkExpr(p,e->rhs))))
    return NULL;

  switch(e->op) {
    // arithmetic -------------------------------------
    case TK_ADD:
    case TK_SUB:
    case TK_MUL:
    case TK_DIV:
      if(lhs_type->tag == rhs_type->tag) {
        if(lhs_type->tag != EPT_INT && lhs_type->tag != EPT_DBL) {
          goto fail;
        }
        e->ctype = lhs_type;
      } else {
        if(BinaryPromotion(p,lhs_type,rhs_type,e))
          goto fail;
        e->ctype = (const Type*)(TypeSysGetDbl(p->tsys));
      }
      break;
    case TK_MOD:
      if(lhs_type->tag == rhs_type->tag && lhs_type->tag == EPT_INT) {
        return (e->ctype = lhs_type);
      } else {
        goto fail;
      }
      break;

    // comparison ------------------------------------
    case TK_LT:
    case TK_LE:
    case TK_GT:
    case TK_GE:
    case TK_EQ:
    case TK_NE:
      if(lhs_type->tag == rhs_type->tag) {
        if(lhs_type->tag != EPT_INT && lhs_type->tag != EPT_DBL && lhs_type->tag == ET_STR)
          goto fail;
      } else {
        if(!BinaryPromotion(p,lhs_type,rhs_type,e))
          goto fail;
      }
      e->ctype = (const Type*)(TypeSysGetBool(p->tsys));
      break;

    // logical ---------------------------------------
    case TK_AND:
    case TK_OR:
      e->lhs = SemaChkToBool(p,e->lhs);
      e->rhs = SemaChkToBool(p,e->rhs);
      e->ctype = (const Type*)(TypeSysGetBool(p->tsys));
      break;
    default:
      assert(0);
      break;
  }

  return e->ctype;

fail:
  SemaChkError(p,(Node*)e,"binary operation %s operand type mismatch",TokenGetStr(e->op));
  return NULL;
}

static const Type* SemaChkTernary( SemaChk* p , NodeTernary* e ) {
  const Type* f_type = SemaChkExpr(p,e->first);
  const Type* s_type = SemaChkExpr(p,e->second);
  const Type* t_type = SemaChkExpr(p,e->third);

  if(!f_type || !s_type || !t_type)
    return NULL;

  if(s_type->tag != t_type->tag) goto fail;

  // convert it to *bool*
  e->first = SemaChkToBool(p,e->first);

  return (e->ctype = s_type);

fail:
  SemaChkError(p,(Node*)e,"ternary operation's second and third argument must have exactly same type");
  return NULL;
}

static const Type* SemaChkExpr( SemaChk* p , Node* e ) {
  switch(e->type) {
    case ENT_LIT:
      return SemaChkLit(p,(NodeLit*)e);
    case ENT_ID:
      return SemaChkId (p,(NodeId*)e);
    case ENT_STRULIT:
      return SemaChkStruLit(p,(NodeStruLit*)e);
    case ENT_DOT:
      return SemaChkDot(p,(NodeDot*)e);
    case ENT_INDEX:
      return SemaChkIndex(p,(NodeIndex*)e);
    case ENT_CALL:
      return SemaChkCall (p,(NodeCall*)e);
    case ENT_UNARY:
      return SemaChkUnary(p,(NodeUnary*)e);
    case ENT_BINARY:
      return SemaChkBinary(p,(NodeBinary*)e);
    case ENT_TERNARY:
      return SemaChkTernary(p,(NodeTernary*)e);
    default:
      assert(0);
      return NULL;
  }
}

// Statement
static int SemaChkStmt ( SemaChk* , Node* );
static int SemaChkChunk( SemaChk* , CodeChunk* );

static int SemaChkLocal( SemaChk* p , NodeLocal* e ) {
  if(p->scp->type != SCPT_LEXICAL) {
    SemaChkError(p,(Node*)e,"current scope cannot define local variable");
    return -1;
  }
  {
    const Type* t = SemaChkGetType(p,&(e->tinfo));
    if(!t) {
      SemaChkError(p,(Node*)e,"type doesn't exist for local variable");
      return -1;
    }
    if(!DefineLVar(p,e->name,t)) {
      SemaChkError(p,(Node*)e,"cannot redefine local variable %s",LitPoolId(p->lpool,e->name));
      return -1;
    }
  }
  return 0;
}

static int SemaChkAssign( SemaChk* p , NodeAssign* e ) {
  if(!SemaChkExpr(p,e->lhs) || !SemaChkExpr(p,e->rhs))
    return -1;
  return 0;
}

static int SemaChkIf( SemaChk* p , NodeIf* e ) {
  for( size_t i = 0 ; i < e->sz ; ++i ) {
    if(e->chains[i].cond && !SemaChkExpr(p,e->chains[i].cond))
      return -1;
    if(SemaChkChunk(p,e->chains[i].chunk))
      return -1;
  }
  return 0;
}

static int SemaChkShortStmt( SemaChk* p ,Node* e ) {
  if(e->type == ENT_LOCAL) {
    return SemaChkLocal(p,(NodeLocal*)e);
  } else {
    return !SemaChkExpr (p,e) ? 0 : -1;
  }
}

static int SemaChkFor( SemaChk* p , NodeFor* e ) {
  LexScp lscp;
  LexScpEnter(p,&lscp);

  if(e->init && SemaChkShortStmt(p,e->init))
    return -1;
  if(e->cond && !SemaChkExpr(p,e->cond))
    return -1;
  if(e->step && SemaChkShortStmt(p,e->step))
    return -1;
  if(SemaChkChunk(p,e->chunk))
    return -1;

  ScpLeave(p);
  return 0;
}

static int SemaChkReturn( SemaChk* p , NodeReturn* e ) {
  if(e->expr && !SemaChkExpr(p,e->expr))
    return -1;
  return 0;
}

static int SemaChkFunc( SemaChk* p , NodeFunc* e ) {
  FuncScp fscp;
  assert(e->ctype);

  FuncScpEnter(p,&fscp,e->ctype);

  for( size_t i = 0 ; i < e->arg_sz ; ++i ) {
    if(!DefineArg(p,e->arg[i].name,e->arg[i].ctype->type)) {
      SemaChkError(p,(Node*)e,"function argument %s redefined",LitPoolId(p->lpool,e->arg[i].name));
      return -1;
    }
  }

  if(SemaChkChunk(p,e->chunk))
    return -1;

  ScpLeave(p);
  return 0;
}

static int SemaChkGlobal( SemaChk* p , NodeGlobal* e ) {
  const Type* t = SemaChkGetType(p,&(e->tinfo));
  if(!t) {
    SemaChkError(p,(Node*)e,"type doesn't exist for global variable");
    return-1;
  }

  if(!SemaChkExpr(p,e->value))
    return -1;

  e->ctype = t;
  return 0;
}

static int SemaChkStmt ( SemaChk* p , Node* e ) {
  switch(e->type) {
    case ENT_LOCAL:
      return SemaChkLocal(p,(NodeLocal*)e);
    case ENT_ASSIGN:
      return SemaChkAssign(p,(NodeAssign*)e);
    case ENT_IF:
      return SemaChkIf(p,(NodeIf*)e);
    case ENT_FOR:
      return SemaChkFor(p,(NodeFor*)e);
    case ENT_CONTINUE:
    case ENT_BREAK:
      return 0;
    case ENT_RETURN:
      return SemaChkReturn(p,(NodeReturn*)e);
    case ENT_FUNC:
      return SemaChkFunc(p,(NodeFunc*)e);
    case ENT_STRUCT_DEF:
      return 0; // handle the during the 1st pass
    case ENT_GLOBAL:
      return SemaChkGlobal(p,(NodeGlobal*)e);
    default:
      break;
  }

  assert(0);
  return -1;
}

static int SemaChkChunk( SemaChk* p, CodeChunk* cc ) {
  LexScp lscp;
  LexScpEnter(p,&lscp);
  for( size_t i = 0 ; i < cc->sz ; ++i ) {
    if(SemaChkStmt(p,cc->stmt[i]))
      return -1;
  }
  ScpLeave(p);
  return 0;
}

PAPI int SemaCheck( LitPool* lpool , TypeSys* tsys , MPool* mpool , const char*     src,
                                                                    NodeFile*      node,
                                                                    const char**    err ) {
  SemaChk sc;
  sc.src = src;
  sc.tsys= tsys;
  sc.lpool= lpool;
  sc.pool = mpool;
  sc.scp = NULL;
  sc.err = NULL;

  // 1. do type registerationg and pre checking
  if(_Chk1(&sc,node)) {
    goto fail;
  }

  // 2. do fully type checking
  {
    GlbScp gscp;
    gscp.base.type = SCPT_GLOBAL;
    gscp.base.prev = NULL;
    SymTableInit(&(gscp.base.stb),mpool);
    sc.scp = (Scp*)(&gscp);

    for( size_t i = 0 ; i < node->chunk->sz ; ++i ) {
      if(SemaChkStmt(&sc,node->chunk->stmt[i]))
        return -1;
    }
    ScpLeave(&sc);
  }

  assert(sc.scp == NULL);
  return 0;

fail:
  assert(sc.err);
  *err = sc.err;
  return -1;
}
