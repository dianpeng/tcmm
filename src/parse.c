#include "tcmm.h"

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>

#define EXPR_MEMPOOL_SIZE 512

#define GRAB(XX) (MPoolGrab((p->pool),sizeof(XX)))
#define L(XX)    (&((XX)->lexer))
#define LEX(XX)  (&(L(XX)->lexeme))
#define NEXT(P)  LexerNext(L(P))

#define EXPECTR(P,TK,STMT)                                               \
  do {                                                                   \
    if((TK) != LEX(P)->tk) {                                             \
      ParserError((P),"expect token %s", TokenGetStr(LEX(p)->tk));       \
      STMT;                                                              \
    }                                                                    \
    NEXT(P);                                                             \
  } while(0)

#define EXPECT(P,TK) EXPECTR(P,TK,return NULL)

static void ParserError( Parser* p , const char* fmt , ... ) {
  char buf[1024];
  va_list vl;
  va_start(vl,fmt);
  vsnprintf(buf,1024,fmt,vl);
  p->err = ReportError("parser",L(p)->src,L(p)->pos,L(p)->nline,L(p)->nchar,buf);
}

PAPI NodePrefixComp* NodePrefixAddComp( NodePrefix* p , MPool* pool ) {
  if(p->comp_sz == p->comp_cap) {
    size_t ncap = p->comp_cap ? p->comp_cap * 2 : 4;
    p->comp = MPoolRealloc(pool,p->comp,sizeof(NodePrefixComp)*p->comp_cap,ncap);
    p->comp_cap = ncap;
  }

  return p->comp + p->comp_sz++;
}

PAPI void ParserInit( Parser* p , LitPool* lpool , TypeSys* tsys , MPool* mpool ,
                                                                   const char* src ) {
  p->lpool = lpool;
  p->tsys  = tsys;
  p->pool  = mpool;
  LexerInit(&(p->lexer),lpool,src);
  LexerNext(&(p->lexer)); // fire the lexer
}

PAPI void ParserDelete( Parser* p ) {
  LexerDelete(&(p->lexer));
}

/** ------------------------------------------------
 * Scope
 * ------------------------------------------------*/
static LexScp* LexScpEnter( Parser* p , LexScp* lscp , int is_loop ) {
  Scp* pscp = p->scp;

  lscp->base.prev = pscp;
  lscp->base.type = SCPT_LEXICAL;
  SymTableInit(&(lscp->base.stb),p->pool);

  if(pscp->type == SCPT_LEXICAL) {
    LexScp* pp = (LexScp*)pscp;
    // accumulated variable size
    lscp->vsz = pp->vsz + pp->base.stb.sz;
    lscp->in_loop = is_loop || pp->in_loop || pp->is_loop;
    lscp->is_loop = is_loop;
    lscp->fscp    = pp->fscp;
  } else {
    assert(pscp->type == SCPT_FUNC);
    assert(is_loop == 0);
    {
      FuncScp* fscp = (FuncScp*)pscp;
      lscp->vsz = 0;
      lscp->in_loop = 0;
      lscp->is_loop = 0;
      lscp->fscp    = fscp;
    }
  }

  // update the current scope context
  p->scp = (Scp*)lscp;

  return lscp;
}

static FuncScp* FuncScpEnter( Parser* p , FuncScp* scp , const FuncType* ft ) {
  assert(p->scp->type == SCPT_GLOBAL);
  {
    GlbScp* pscp = (GlbScp*)(p->scp);
    scp->base.prev = (Scp*)pscp;
    scp->base.type = SCPT_FUNC;
    SymTableInit(&(scp->base.stb),p->pool);
    scp->type = ft;
    scp->max_stksz = 0;

    p->scp = (Scp*)scp;
    return scp;
  }
}

static Scp* ScpLeave( Parser* p ) {
  Scp* scp = p->scp;

  if(scp->type == SCPT_LEXICAL) {
    LexScp* lscp = (LexScp*)(scp);

    size_t max_vsz = lscp->vsz + lscp->base.stb.sz;
    if(max_vsz > lscp->fscp->max_stksz)
      lscp->fscp->max_stksz = max_vsz;
  }
  SymTableDelete(&(scp->stb));
  p->scp = scp->prev;
  return p->scp;
}

static inline FuncScp* CurFScp( Parser* p ) {
  assert( p->scp && p->scp->type == SCPT_FUNC );
  return (FuncScp*)(p->scp);
}

static inline LexScp* CurLScp ( Parser* p ) {
  assert( p->scp && p->scp->type == SCPT_LEXICAL );
  return (LexScp*)(p->scp);
}

static inline GlbScp* CurGScp ( Parser* p ) {
  assert( p->scp && p->scp->type == SCPT_GLOBAL );
  return (GlbScp*)(p->scp);
}

static inline FuncScp* FScp( Parser* p ) {
  if(p->scp->type == SCPT_FUNC)
    return (FuncScp*)p->scp;
  if(p->scp->type == SCPT_LEXICAL)
    return ((LexScp*)(p->scp))->fscp;

  return NULL;
}

// resolve a symbol name based on a literal index
static inline const Sym* FindSym( Parser* p , LitIdx name ) {
  Scp* cscp = p->scp;
  while(cscp) {
    const Sym* ret = SymTableGet(&(cscp->stb),name);
    if(ret) return ret;
    cscp = cscp->prev;
  }
  return NULL;
}

static inline LVar* DefineLVar( Parser* p , LitIdx name , const Type* t ) {
  assert(p->scp->type == SCPT_LEXICAL);
  if(SymTableGetLVar(&(p->scp->stb),name))
    return NULL;
  return SymTableSetLVar(&(p->scp->stb),name,t);
}

static inline GVar* DefineGVar( Parser* p , LitIdx name , const Type* t ) {
  assert(p->scp->type == SCPT_GLOBAL);
  if(SymTableGetGVar(&(p->scp->stb),name))
    return NULL;
  return SymTableSetGVar(&(p->scp->stb),name,t);
}

static inline Arg* DefineArg( Parser* p , LitIdx name , const Type* t ) {
  assert(p->scp->type == SCPT_FUNC);
  if(SymTableGetArg(&(p->scp->stb),name))
    return NULL;
  return SymTableSetArg(&(p->scp->stb),name,t);
}

/** ------------------------------------------------
 * Expression Parsing
 * ------------------------------------------------*/
static Node* ParseExpr   ( Parser* );
static Node* ParseTernary( Parser* );
static Node* ParseBinary ( Parser* );
static Node* ParseUnary  ( Parser* );
static Node* ParsePrefix ( Parser* );
static Node* ParsePrimary( Parser* );
static Node* ParseStruLit( Parser* );

static inline void NodeDbg0( Parser* p , Node* n ) {
  n->dbg_start = L(p)->pos - LEX(p)->tk_sz;
  n->dbg_end   = L(p)->pos;
}

static inline int _IsPrefixOp( Token tk ) {
  return tk == TK_DOT || tk == TK_LSQR || tk == TK_LPAR;
}

static inline int _IsUnaryOp( Token tk ) {
  return tk == TK_NOT || tk == TK_SUB;
}

static Node* ParseStruLit( Parser* p ) {
  NodeStruLit* slit;
  const StructType* t;
  size_t pos_start = L(p)->pos;

  assert( LEX(p)->tk == TK_STRUCT );
  NEXT(p);

  // get the type object w.r.t the type name
  if(LEX(p)->tk != TK_ID) {
    ParserError(p,"expect a ID to indicate type name");
    return NULL;
  }
  if(!(t = TypeSysGetStruct(p->tsys,LEX(p)->lit))) {
    ParserError(p,"struct type %s is not defined",LitPoolId(p->lpool,LEX(p)->lit));
    return NULL;
  }
  NEXT(p); // skip the type name
  EXPECT(p,TK_LBRA); // expect a {

  // create the NodeStruLit object
  slit = GRAB(NodeStruLit);
  slit->base.type      = ENT_STRULIT;
  slit->base.dbg_start = pos_start;

  slit->ctype  = t;
  slit->assign = NULL;

  // parsing struct literal field
  if(LEX(p)->tk == TK_RBRA) {
    NEXT(p);
  } else {
    do {
      NodeStruLitAssign* field = NULL;
      const FieldType*   ft    = NULL;
      const StructType*  st    = (const StructType*)(t);

      EXPECT(p,TK_DOT); // expect a dot
      if(LEX(p)->tk != TK_ID) {
        ParserError(p,"expect a ID to indicate field name of struct");
        return NULL;
      }

      field = GRAB(NodeStruLitAssign);

      // NOTES: the order of the assign is actually
      //        been chained in reverse order
      field->next = slit->assign;
      slit->assign= field;

      if(!(ft = TypeSysGetStructField(p->tsys,st,LEX(p)->lit))) {
        ParserError(p,"field %s is not found in struct %s",LitPoolId(p->lpool,LEX(p)->lit),
                                                           LitPoolId(p->lpool,t->name));
        return NULL;
      }

      field->ftype = ft;

      NEXT(p);             // skip ID
      EXPECT(p,TK_ASSIGN); // skip =

      if(!(field->value = ParseExpr(p))) return NULL;
      switch(LEX(p)->tk) {
        case TK_COMMA:
          NEXT(p);
          break;
        case TK_RBRA:
          NEXT(p);
          goto done;
        default:
          ParserError(p,"expect a \",\" or \"}\" in struct literal");
          return NULL;
      }

    } while(1);

  }

done:
  slit->base.dbg_end = L(p)->pos;
  return (Node*)slit;
}

static Node* ParsePrimary( Parser* p ) {
  switch(LEX(p)->tk) {
    case TK_LIT_INT:
    case TK_LIT_DBL:
    case TK_LIT_STR:
    case TK_LIT_CHAR:
    case TK_LIT_TRUE:
    case TK_LIT_FALSE:
      {
        NodeLit* n = GRAB(NodeLit);
        n->base.type = ENT_LIT;
        NodeDbg0(p,(Node*)n);
        if(LEX(p)->tk == TK_LIT_TRUE || LEX(p)->tk == TK_LIT_FALSE) {
          n->lit = LEX(p)->tk == TK_LIT_TRUE ? LitPoolGetTrue(p->lpool) : LitPoolGetFalse(p->lpool);
        } else {
          n->lit =  LEX(p)->lit;
        }
        NEXT(p);
        return (Node*)n;
      }
    case TK_LPAR:
      {
        Node* n;
        size_t dbg_start = L(p)->pos;

        NEXT(p);
        if(!(n = ParseExpr(p)))
          return NULL;
        EXPECT(p,TK_LPAR);

        // adjust debug info to include the "(" and ")"
        n->dbg_start = dbg_start;
        n->dbg_end   = L(p)->pos;
        return (Node*)n;
      }

    case TK_ID:
      {
        NodeId* n = GRAB(NodeId);
        n->base.type = ENT_ID;
        NodeDbg0(p,(Node*)n);
        n->name = LEX(p)->lit;
        NEXT(p);
        return (Node*)n;
      }

    case TK_STRUCT:
      return ParseStruLit(p);

    default:
      ParserError(p,"expect a valid primary expression , eg literal , subexpression or struct literal");
      return NULL;
  }
}

static NodePrefixComp* ParseCall( Parser* p , NodePrefix* pp ) {
  size_t dbg_start = L(p)->pos;

  NodePrefixComp* c = NodePrefixAddComp(pp,(p->pool));
  c->comp_type = EEPCT_CALL;
  c->c.call    = GRAB(NodePrefixCompCall);
  c->c.call->sz= 0;

  NEXT(p); // skip (
  if(LEX(p)->tk == TK_RPAR) {
    NEXT(p);
  } else {
    size_t pos = 0;

    do {
      Node* e;
      if(pos == CONFIG_MAX_CALL_ARGS) {
        ParserError(p,"too many arguments for function call, more than %d",CONFIG_MAX_CALL_ARGS);
        return NULL;
      }

      if(!(e = ParseExpr(p))) return NULL;
      c->c.call->args[pos++] = e;

      switch(LEX(p)->tk) {
        case TK_COMMA:
          NEXT(p);
          break;
        case TK_RPAR:
          NEXT(p);
          goto done;
        default:
          ParserError(p,"expect a \",\" or \")\" here");
          break;
      }
    } while(1);
  }

done:
  c->c.call->dbg_start = dbg_start;
  c->c.call->dbg_end   = L(p)->pos;
  return c;
}

static Node* ParsePrefix( Parser* p ) {
  size_t dbg_start = L(p)->pos;
  Node* pexpr = ParsePrimary(p);
  if(!pexpr) return NULL;

  if(pexpr->type == ENT_ID && _IsPrefixOp(LEX(p)->tk)) {
    NodeId*         id = (NodeId*)pexpr;
    NodePrefix* prefix = GRAB(NodePrefix);
    prefix->base.type      = ENT_PREFIX;
    prefix->base.dbg_start = dbg_start;

    /**
     * now at here, we meet a ID which is a possible start of a prefix
     * expression.
     */
    for( ;; ) {
      NodePrefixComp* c;
      switch(LEX(p)->tk) {
        case TK_DOT:
          NEXT(p); // skip .
          if(LEX(p)->tk != TK_ID) {
            ParserError(p,"expect a ID here");
            return NULL;
          }
          c            = NodePrefixAddComp(prefix,(p->pool));
          c->comp_type = EEPCT_DOT;
          c->c.name    = LEX(p)->lit;
          NEXT(p);
          break;
        case TK_LSQR:
          NEXT(p); // skip [
          c = NodePrefixAddComp(prefix,(p->pool));
          if(!(c->c.idx = ParseExpr(p))) return NULL;
          c->comp_type= EEPCT_IDX;
          EXPECT(p,TK_RSQR);
          break;
        case TK_LPAR:
          if(!(c = ParseCall(p,prefix))) return NULL;
          break;
        default:
          goto done;
      }
    }

done:
    prefix->init         = id->name;
    prefix->base.dbg_end = L(p)->pos;
    return (Node*)prefix;
  }

  return pexpr;
}

static Node* ParseUnary( Parser* p ) {
  size_t dbg_start = L(p)->pos;

  if(_IsUnaryOp(LEX(p)->tk)) {
    Token op = LEX(p)->tk;
    Node* opr;
    NodeUnary* una;
    NEXT(p);
    if(!(opr = ParseUnary(p))) return NULL;

    una = GRAB(NodeUnary);
    una->base.dbg_start = dbg_start;
    una->base.dbg_end   = L(p)->pos;
    una->base.type      = ENT_UNARY;
    una->opr = opr;
    una->op  = op;
    return (Node*)una;
  }
  return ParsePrefix(p);
}

/**
 * Binary parser , precedence climbing style
 */

static inline int _GetBinOpPrec( Token tk ) {
  switch(tk) {
    case TK_MUL:
    case TK_DIV:
    case TK_MOD:
      return 1;
    case TK_ADD:
    case TK_SUB:
      return 2;
    case TK_LT :
    case TK_LE :
    case TK_GT :
    case TK_GE :
      return 3;
    case TK_EQ :
    case TK_NE :
      return 4;
    case TK_AND:
      return 5;
    case TK_OR:
      return 6;
    default:
      return -1;
  }
}

static inline int _GetBinOpMaxPrec() { return 6; }

static Node* _ParseBin( Parser* p , int prec ) {
  if(prec == 0)
    return ParseUnary(p);
  else {
    size_t dbg_start = L(p)->pos;
    Node* lhs;
    Node* rhs;
    int cp;

    if(!(lhs = _ParseBin(p,prec-1)))
      return NULL;

    for( ;; ) {
      if((cp = _GetBinOpPrec(LEX(p)->tk)) <0)
        return lhs;
      if(cp == prec) {
        int op = LEX(p)->tk;
        NodeBinary* bin;

        NEXT(p);
        if(!(rhs = _ParseBin(p,prec-1)))
          return NULL;

        bin                 = GRAB(NodeBinary);
        bin->base.type      = ENT_BINARY;
        bin->base.dbg_start = dbg_start;
        bin->lhs            = lhs;
        bin->rhs            = rhs;
        bin->op             = op;
        lhs                 = (Node*)bin;
      } else {
        assert(prec < cp);
        goto done;
      }
    }

done:
    return lhs;
  }
}

static Node* ParseBinary( Parser* p ) {
  return _ParseBin(p,_GetBinOpMaxPrec());
}

static Node* ParseTernary( Parser* p ) {
  size_t dbg_start = L(p)->pos;
  Node* f = ParseBinary(p);
  if(!f) return NULL;

  if(LEX(p)->tk == TK_QUESTION) {
    NodeTernary* ten;
    Node* cond;
    Node* trd ;

    NEXT(p);
    if(!(cond = ParseBinary(p)))
      return NULL;

    EXPECT(p,TK_COLON);

    if(!(trd  = ParseBinary(p)))
      return NULL;

    ten = GRAB(NodeTernary);
    ten->base.type      = ENT_TERNARY;
    ten->base.dbg_start = dbg_start;
    ten->base.dbg_end   = L(p)->pos;
    ten->first = f;
    ten->second= cond;
    ten->third = trd;

    return (Node*)ten;
  }
  return f;
}

static Node* ParseExpr( Parser* p ) { return ParseTernary(p); }

/** ----------------------------------------------------------------
 * Statment
 * ----------------------------------------------------------------*/
// Parse a declaration. A declaration is something as following :
// type id array-modifier
static int ParseVDec( Parser* , const Type** , LitIdx* ref );

static NodeChunk* ParseChunk       ( Parser* , int );
static NodeChunk* ParseChunkOrSStmt( Parser* , int );

static Node* ParseLocal       ( Parser* );
static Node* ParseCallOrAssign( Parser* );
static Node* ParseIf          ( Parser* );
static Node* ParseFor         ( Parser* );
static Node* ParseContinue    ( Parser* );
static Node* ParseBreak       ( Parser* );
static Node* ParseReturn      ( Parser* );
static Node* ParseStmt        ( Parser* );

static int ParseVDec( Parser* p , const Type** out_tp , LitIdx* ref ) {
  switch(LEX(p)->tk) {
    case TK_INT:
      *out_tp = (const Type*)TypeSysGetInt(p->tsys);
      break;
    case TK_DBL:
      *out_tp = (const Type*)TypeSysGetDbl(p->tsys);
      break;
    case TK_BOOL:
      *out_tp = (const Type*)TypeSysGetBool(p->tsys);
      break;
    case TK_CHAR:
      *out_tp = (const Type*)TypeSysGetChar(p->tsys);
      break;
    case TK_STR:
      *out_tp = (const Type*)TypeSysGetStr (p->tsys);
      break;
    case TK_STRUCT:
      {
        LitIdx tid;
        NEXT(p);
        if(LEX(p)->tk != TK_ID) {
          ParserError(p,"expect a ID name after the struct");
          return -1;
        }
        tid = LEX(p)->lit;
        *out_tp = (const Type*)TypeSysGetStruct(p->tsys,tid);
        if(!*out_tp) {
          ParserError(p,"struct type %s is not found",LitPoolId(p->lpool,tid));
          return -1;
        }
      }
      break;
    default:
      ParserError(p,"void type cannot be used for declare variable");
      return -1;
  }

  NEXT(p); // skip the whole type name

  if(LEX(p)->tk != TK_ID) {
    ParserError(p,"expect a ID name after the type");
    return -1;
  }

  *ref = LEX(p)->lit;
  NEXT(p); // skip the name

  // try to check if there's any trailing array specifier
  if(LEX(p)->tk == TK_LSQR) {
    size_t len;

    // this is an array type since it has a array modifier
    NEXT(p); // skip [
    if(LEX(p)->tk != TK_LIT_INT) {
      ParserError(p,"expect a literal integer serve as array length");
      return -1;
    }

    len = (size_t)LitPoolInt(p->lpool,LEX(p)->lit);
    if(len == 0) {
      ParserError(p,"array length cannot be 0");
      return -1;
    }

    // for array if it is not defined, then just try to define it
    {
      const ArrType* at = TypeSysGetArr(p->tsys,*out_tp,len);
      if(!at) {
        at = TypeSysSetArr(p->tsys,*out_tp,len);
        *out_tp = (const Type*)(at);
      }
    }

    NEXT(p);
    EXPECTR(p,TK_RSQR,return -1);
  }

  return 0;
}


// Parsing local variable definition or declaration.
// lvar := def | dec
// def  := type id array-modifier ';'
// dec  := type id array-modifier '=' expr ';'
static Node* ParseLocal( Parser* p ) {
  const Type* tp;
  LitIdx      vid;
  size_t      dbg_start = L(p)->pos;
  NodeLocal*  l;

  if(ParseVDec(p,&tp,&vid)) return NULL;

  l = GRAB(NodeLocal);

  l->base.type      = ENT_LOCAL;
  l->base.dbg_start = dbg_start;
  l->base.dbg_end   = L(p)->pos;
  l->ctype          = tp;
  l->name           = vid;
  l->rhs            = NULL;

  // now check whether there's trailing assignment or not
  if(LEX(p)->tk == TK_ASSIGN) {
    NEXT(p); // skip =
    if(!(l->rhs = ParseExpr(p)))
      return NULL;
  }

  EXPECT(p,TK_SEMICOLON); // skip ;
  return (Node*)l;
}

static int IsNodeLHS( Node* n ) {
  if(n->type == ENT_ID || n->type == ENT_PREFIX) {
    if(n->type == ENT_PREFIX) {
      NodePrefix* np = (NodePrefix*)(n);
      // function call cannot be used as LHS
      if(np->comp[np->comp_sz-1].comp_type == EEPCT_CALL) {
        goto not;
      }
    }
    return 1;
  }

not:
  return 0;
}

static int IsAssignOp( Token tk ) {
  return tk == TK_ASSIGN ||
         tk == TK_SADD   ||
         tk == TK_SSUB   ||
         tk == TK_SMUL   ||
         tk == TK_SDIV;
}

static Node* ParseCallOrAssign( Parser* p ) {
  size_t dbg_start = L(p)->pos;

  Node* lhs = ParseExpr(p); // try to parse if as a lhs expression
  if(IsAssignOp(LEX(p)->tk)) {
    NodeAssign* n;
    Node*     rhs;
    Token      op = LEX(p)->tk;

    if(!IsNodeLHS(lhs)) {
      ParserError(p,"not valid left hand side, cannot be used as assignment");
      return NULL;
    }

    NEXT(p); // skip assignment token
    if(!(rhs = ParseExpr(p)))
      return NULL;
    EXPECT(p,TK_SEMICOLON);

    n = GRAB(NodeAssign);

    n->base.type = ENT_ASSIGN;
    n->base.dbg_start = dbg_start;
    n->base.dbg_end   = L(p)->pos;
    n->lhs            = lhs;
    n->rhs            = rhs;
    n->op             = op;

    return (Node*)n;
  } else {
    // now this statement *must* be a call otherwise it is not allowed
    if(lhs->type == ENT_PREFIX) {
      NodePrefix* npref = (NodePrefix*)(lhs);
      if(npref->comp[npref->comp_sz-1].comp_type == EEPCT_CALL) {
        EXPECT(p,TK_SEMICOLON);
        return lhs;
      }
    }

    ParserError(p,"meaningless statement, expect a call");
    return NULL;
  }
}

static int ParseBranch( Parser* p , NodeIf* n , int is_else ) {
  Node* cond = NULL;
  NodeIfBranch* if_br;

  if(!is_else) {
    // try to parse the condition
    if(LEX(p)->tk == TK_LPAR) {
      if(!(cond = ParseExpr(p))) return -1;
      EXPECTR(p,TK_RPAR,return -1);
    }
  }

  if(n->sz == n->cap) {
    size_t ncap = n->cap ? 2 * n->cap : 2;
    n->chains = MPoolRealloc(p->pool,n->chains,sizeof(NodeIfBranch)*n->sz,sizeof(NodeIfBranch)*ncap);
    n->cap    = ncap;
  }

  if_br = n->chains + n->sz++;
  if(!(if_br->chunk = ParseChunkOrSStmt(p,0)))
    return -1;
  if_br->cond = cond;

  return 0;
}

static Node* ParseIf( Parser* p ) {
  size_t dbg_start = L(p)->pos;
  NodeIf* n        = GRAB(NodeIf);

  n->base.type = ENT_IF;
  n->base.dbg_start = dbg_start;

  // parse the leading if
  NEXT(p); // skip if
  if(!ParseBranch(p,n,0))
    return NULL;

  // elif cluster
  while(LEX(p)->tk == TK_ELIF) {
    NEXT(p);
    if(!ParseBranch(p,n,0)) return NULL;
  }

  // dangling else
  if(LEX(p)->tk == TK_ELSE) {
    NEXT(p);
    if(!ParseBranch(p,n,1)) return NULL;
  }

  n->base.dbg_end = L(p)->pos;
  return (Node*)n;
}

// A short-stmt is a stmt that allows following stuff:
// 1. a normal statment that does *call* or assignment
// 2. a declaration/definition
static Node* ParseShortStmt( Parser* p ) {
  switch(LEX(p)->tk) {
    case TK_INT:
    case TK_DBL:
    case TK_VOID:
    case TK_CHAR:
    case TK_BOOL:
    case TK_STR:
    case TK_STRUCT:
      return ParseLocal(p);
    default:
      return ParseCallOrAssign(p);
  }
}

static Node* ParseFor( Parser* p ) {
  size_t  dbg_start = L(p)->pos;
  LexScp   lscp;
  NodeFor* ret;

  Node* init = NULL;
  Node* cond = NULL;
  Node* step = NULL;
  NodeChunk* ck = NULL;

  NEXT(p); EXPECT(p,TK_LPAR);

  // setup a new lexical scope
  LexScpEnter(p,&lscp,1);

  // initialize stmt
  if(LEX(p)->tk == TK_SEMICOLON)
    NEXT(p);
  else {
    // this function eats the semicolon
    if(!(init = ParseShortStmt(p)))
      return NULL;
  }

  if(LEX(p)->tk == TK_SEMICOLON)
    NEXT(p);
  else {
    if(!(cond = ParseExpr(p))) return NULL;
    EXPECT(p,TK_SEMICOLON);
  }

  if(LEX(p)->tk == TK_RPAR)
    NEXT(p);
  else {
    if(!(step = ParseExpr(p))) return NULL;
    EXPECT(p,TK_RPAR);
  }

  if(!(ck = ParseChunkOrSStmt(p,1)))
    return NULL;

  ret = GRAB(NodeFor);
  ret->base.type = ENT_FOR;
  ret->base.dbg_start = dbg_start;
  ret->base.dbg_end   = L(p)->pos;

  ret->init = init;
  ret->cond = cond;
  ret->step = step;
  ret->chunk= ck;

  ScpLeave(p);

  return (Node*)ret;
}

static Node* ParseContinue( Parser* p ) {
  NodeContinue* c;
  if(!CurLScp(p)->in_loop) {
    ParserError(p,"cannot write continue statement in none-loop scope");
    return NULL;
  }
  c = GRAB(NodeContinue);
  c->base.type      = ENT_CONTINUE;
  c->base.dbg_start = L(p)->pos;

  NEXT(p); EXPECT(p,TK_SEMICOLON);
  c->base.dbg_end   = L(p)->pos;
  return (Node*)c;
}

static Node* ParseBreak( Parser* p ) {
  NodeBreak* b;
  if(!CurLScp(p)->in_loop) {
    ParserError(p,"cannot write break statement in none-loop scope");
    return NULL;
  }

  b = GRAB(NodeBreak);
  b->base.type      = ENT_BREAK;
  b->base.dbg_start = L(p)->pos;

  NEXT(p); EXPECT(p,TK_SEMICOLON);

  b->base.dbg_end   = L(p)->pos;
  return (Node*)b;
}

static Node* ParseReturn( Parser* p ) {
  size_t dbg_start = L(p)->pos;
  NodeReturn* r;
  Node*       rv;
  FuncScp*    fscp = FScp(p);

  assert(fscp);
  NEXT(p); // skip return

  if(LEX(p)->tk == TK_SEMICOLON) {
    if(fscp->type->ret->tag != EPT_VOID) {
      ParserError(p,"cannot return nothing in a function with return type not defined as void");
      return NULL;
    }
  } else {
    if(!(rv = ParseExpr(p)))
      return NULL;
    if(fscp->type->ret->tag == EPT_VOID) {
      ParserError(p,"cannot return a value in function defined to have a none-void return type");
      return NULL;
    }
  }

  EXPECT(p,TK_SEMICOLON);
  r = GRAB(NodeReturn);

  r->base.type      = ENT_RETURN;
  r->base.dbg_start = dbg_start;
  r->base.dbg_end   = L(p)->pos;
  r->expr           = rv;

  return (Node*)r;
}

static NodeChunk* ParseChunk( Parser* p , int is_loop ) {
  NodeChunk* ret;
  LexScp scp;

  LexScpEnter(p,&scp,is_loop);

  assert(LEX(p)->tk == TK_LBRA);
  NEXT(p);

  ret = GRAB(NodeChunk);
  ret->stmt = NULL;
  ret->sz   = 0;
  ret->cap  = 0;

  if(LEX(p)->tk != TK_RBRA) {
    for( ;; ) {
      Node* stmt = ParseStmt(p);
      if(!stmt) return NULL;
      if(ret->sz == ret->cap) {
        size_t ncap = ret->cap ? ret->cap * 2 : 8;
        ret->stmt = MPoolRealloc(p->pool,ret->stmt,sizeof(Node*)*ret->sz,ncap*sizeof(Node*));
        ret->cap  = ncap;
      }
      ret->stmt[ret->sz++] = stmt;

      if(LEX(p)->tk == TK_RBRA) {
        break;
      }
    }
  }

  NEXT(p); // skip the }

  ScpLeave(p);
  return ret;
}

static NodeChunk* ParseChunkOrSStmt( Parser* p , int is_loop ) {
  if(LEX(p)->tk == TK_RBRA) {
    return ParseChunk(p,is_loop);
  } else {
    LexScp scp;
    NodeChunk* ret = GRAB(NodeChunk);
    Node* stmt;

    LexScpEnter(p,&scp,is_loop);

    if(!(stmt = ParseStmt(p)))
        return NULL;

    ret->stmt = MPoolGrab(p->pool,sizeof(Node*));
    ret->stmt[0] = stmt;
    ret->cap     = 1;
    ret->sz      = 1;

    ScpLeave(p);
    return ret;
  }
}

static Node* ParseStmt( Parser* p ) {
  switch(LEX(p)->tk) {
    case TK_IF:
      return ParseIf(p);
    case TK_FOR:
      return ParseFor(p);
    case TK_CONTINUE:
      return ParseContinue(p);
    case TK_BREAK:
      return ParseBreak(p);
    case TK_RETURN:
      return ParseReturn(p);
    case TK_INT:
    case TK_DBL:
    case TK_VOID:
    case TK_CHAR:
    case TK_STR:
    case TK_BOOL:
    case TK_STRUCT:
      return ParseLocal(p);
    default:
      return ParseCallOrAssign(p);
  }
}

static int ParseFuncArgList( Parser* p , FuncType* ft , FuncScp* fscp ) {
  NEXT(p); // skip (

  if(LEX(p)->tk == TK_RPAR) {
    NEXT(p);
  } else {
    for( ;; ) {
      LitIdx      aname;
      const Type* atype;
      if(ft->arg_size == CONFIG_MAX_CALL_ARGS) {
        ParserError(p,"too many function arguments, only allowed %d",CONFIG_MAX_CALL_ARGS);
        return -1;
      }
      if(ParseVDec(p,&atype,&aname)) return -1;
      TypeSysFuncAddArg(p->tsys,ft,atype,aname);
      SymTableSetArg(&(fscp->base.stb),aname,atype);

      switch(LEX(p)->tk) {
        case TK_COMMA:
          NEXT(p);
          break;
        case TK_RPAR:
          NEXT(p);
          goto done;
        default:
          ParserError(p,"expect \",\" or \")\" in argument list");
          return -1;
      }
    }
  }
done:
  return 0;
}

static Node* ParseFunction( Parser* p , LitIdx fname , const Type* rtype ) {
  assert(LEX(p)->tk == TK_LPAR);
  {
    size_t dbg_start = L(p)->pos;
    NodeFunction* fnode;
    FuncScp fscp;
    FuncType* ft = TypeSysSetFunc(p->tsys,fname);

    FuncScpEnter(p,&fscp,ft);

    if(ParseFunArgList(p,ft,&fscp)) return NULL;
    if(LEX(p)->tk != TK_LBRA) {
      ParserError(p,"expect a \"{\" to start a function body");
      return NULL;
    }

    fnode                 = GRAB(NodeFunction);
    fnode->base.type      = ENT_FUNCT;
    fnode->base.dbg_start = dbg_start;
    fnode->type           = ft;

    if(!(fnode->chunk = ParseChunk(p,0)))
      return NULL;

    fnode->base.dbg_end   = L(p)->pos;

    ScpLeave(p);
    return (Node*)fnode;
  }
}

static Node* ParseGlbVarOrFunction( Parser* p ) {
}

static Node* ParseStruct( Parser* p ) {
}

static Node* ParseFile( Parser* p ) {
  GlbScp scp;
  Node* stmt;
  NodeFile* all       = GRAB(NodeFile);
  all->base.type      = ENT_FILE;
  all->base.dbg_start = L(p)->pos;
  all->chunk          = GRAB(NodeChunk);
  all->chunk->stmt    = NULL;
  all->chunk->sz      = 0;
  all->chunk->cap     = 0;

  scp.base.type = SCPT_GLOBAL;
  scp.base.prev = NULL;
  p->scp        = (Scp*)scp;

  SymTableInit(&(scp.base.stb),p->pool);

  for( ;; ) {
    switch(LEX(p)->tk) {
      case TK_VOID:
      case TK_INT:
      case TK_DBL:
      case TK_CHAR:
      case TK_BOOL:
      case TK_STR:
        if(!(stmt = ParseGlbVarOrFunction(p)))
          return NULL;
        break;
      case TK_STRUCT:
        if(!(stmt = ParseStruct(p)))
          return NULL;
        break;
      default:
        ParserError(p,"unknown global scope statement");
        return NULL;
    }

    if(all->chunk->cap == all->chunk->sz) {
      size_t ncap = all->chunk->cap ? 2 * all->chunk->cap : 4;
      all->chunk->stmt = MPoolRealloc(p->pool,
                                      all->chunk->stmt,
                                      sizeof(Node*)*all->chunk->sz,
                                      sizeof(Node*)*ncap);
      all->chunk->cap  = ncap;
    }
    all->chunk->stmt[all->chunk->sz++] = stmt;
  }

  SymTableDelete(&(scp.base.stb));

  assert(p->scp == NULL);
}

/** ----------------------------------------------------------------
 * ToJSON
 * ----------------------------------------------------------------*/
static void _NodeToJSON  ( Parser* p, FILE* , const Node* );
static void _LitToJSON   ( Parser* p, FILE* , const NodeLit*    );
static void _IdToJSON    ( Parser* p, FILE* , const NodeId*     );
static void _SLitToJSON  ( Parser* p, FILE* , const NodeStruLit*);
static void _PrefixToJSON( Parser* p, FILE* , const NodePrefix* );
static void _UnaToJSON   ( Parser* p, FILE* , const NodeUnary*  );
static void _BinToJSON   ( Parser* p, FILE* , const NodeBinary* );
static void _TenToJSON   ( Parser* p, FILE* , const NodeTernary* );

// Dump the expression into JSON format for better visualization or
// unittest purpose since JSON is a universally parsable format and
// it is easy to hook it into other toolings
void NodeToJSON( Parser* p, FILE* output , const Node* e ) {
  _NodeToJSON(p,output,e);
}

static void _NodeToJSON( Parser* p, FILE* f, const Node* e ) {
  switch(e->type) {
    case ENT_LIT: _LitToJSON(p,f,(const NodeLit*)e); break;
    case ENT_ID:  _IdToJSON (p,f,(const NodeId* )e); break;
    case ENT_STRULIT: _SLitToJSON(p,f,(const NodeStruLit*)e); break;
    case ENT_PREFIX: _PrefixToJSON(p,f,(const NodePrefix*)e); break;
    case ENT_UNARY:  _UnaToJSON(p,f,(const NodeUnary*)e); break;
    case ENT_BINARY: _BinToJSON(p,f,(const NodeBinary*)e); break;
    case ENT_TERNARY: _TenToJSON(p,f,(const NodeTernary*)e); break;
    default: assert(0); break;
  }
}

static void _PrintEscChar( FILE* f , int c ) {
  switch(c) {
    case '\n': fprintf(f,"\\n"); break;
    case '\t': fprintf(f,"\\t"); break;
    case '\v': fprintf(f,"\\v"); break;
    case '\r': fprintf(f,"\\r"); break;
    case '\b': fprintf(f,"\\b"); break;
    case '"':  fprintf(f,"\\\"");break;
    case '\'': fprintf(f,"\\'"); break;
    default: fprintf(f,"%c",c);  break;
  }
}

static void _PrintEscStr( FILE* f , const char* str ) {
  fprintf(f,"\"");
  for( ;*str; ++str ) {
    _PrintEscChar(f,*str);
  }
  fprintf(f,"\"");
}

static void _PrintRawType( Parser* p , FILE* f , const Type* t ) {
  switch(t->tag) {
    case ET_STR:
      fprintf(f,"str");
      break;
    case ET_ARR:
      {
        const ArrType* at = (const ArrType*)(t);
        _PrintRawType(p,f,at->type);
        fprintf(f,"[%d]",(int)at->len);
      }
      break;
    case ET_STRUCT:
      {
        const StructType* st = (const StructType*)(t);
        fprintf(f,"struct %s",LitPoolId(p->lpool,st->name));
      }
      break;
    case ET_FUNC:
      {
        const FuncType* ft = (const FuncType*)(t);
        fprintf(f,"func %s",LitPoolId(p->lpool,ft->name));
      }
      break;
    case EPT_INT: fprintf(f,"int"); break;
    case EPT_DBL: fprintf(f,"dbl"); break;
    case EPT_CHAR: fprintf(f,"char"); break;
    case EPT_BOOL: fprintf(f,"bool"); break;
    case EPT_VOID: fprintf(f,"void"); break;
    default: break;
  }
}

static void _PrintType( Parser* p , FILE* f , const Type* t ) {
  fprintf(f,"\""); _PrintRawType(p,f,t); fprintf(f,"\"");
}

static void _LitToJSON( Parser* p, FILE* f , const NodeLit* lit ) {
  const Lit* l = LitPoolIndex(p->lpool,lit->lit);
  switch(l->type) {
    case ELT_INT:   fprintf(f,"%d",l->d.ival); break;
    case ELT_DBL:   fprintf(f,"%f",l->d.rval); break;
    case ELT_STR:   _PrintEscStr(f,l->d.str); break;
    case ELT_CHAR:  _PrintEscChar(f,l->d.cval); break;
    case ELT_TRUE:  fprintf(f,"true"); break;
    case ELT_FALSE: fprintf(f,"false"); break;
    default: assert(0); break;
  }
}

static void _IdToJSON( Parser* p , FILE* f , const NodeId* id ) {
  fprintf(f,"{ \"type\" : \"id\" , \"value\" : \"%s\" }",LitPoolId(p->lpool,id->name));
}

static void _SLitToJSON( Parser* p , FILE* f , const NodeStruLit* slit ) {
  fprintf(f,"{ \"type\" : \"struct-literal\" , \"name\" : \"%s\" , value : [ " , LitPoolId(p->lpool,slit->ctype->name));
  {
    NodeStruLitAssign* field = slit->assign;
    for( ; field ; field = field->next ) {
      fprintf(f,"{ \"type\" : " );
      _PrintType(p,f,(const Type*)field->ftype);
      fprintf(f,", \"value\" : ");
      _NodeToJSON(p,f,field->value);
      fprintf(f,"}");
      if(field->next != NULL) fprintf(f,",");
    }
  }
  fprintf(f,"]}");
}

static void _PrefixToJSON( Parser* p , FILE* f , const NodePrefix* e ) {
  fprintf(f,"{ \"type\" : \"prefix\" , \"init\" : \"%s\" ",LitPoolId(p->lpool,e->init));
  fprintf(f,", \"rest\" : [");
  for( size_t i = 0 ; i < e->comp_sz ; ++i ) {
    const NodePrefixComp* c = e->comp + i;
    switch(c->comp_type) {
      case EEPCT_DOT:
        fprintf(f,"{ \"type\" : \"dot\" , \"name\" : \"%s\" }",LitPoolId(p->lpool,c->c.name));
        break;
      case EEPCT_IDX:
        fprintf(f,"{ \"type\" : \"index\" , \"value\" : ");
        _NodeToJSON(p,f,c->c.idx);
        fprintf(f,"}");
        break;
      case EEPCT_CALL:
        fprintf(f,"{ \"type\" : \"call\" , \"arg\" : [");
        for( size_t i = 0 ; i < c->c.call->sz; ++i ) {
          _NodeToJSON(p,f,c->c.call->args[i]);
          if(i < c->c.call->sz-1) fprintf(f,",");
        }
        fprintf(f,"]}");
        break;
      default:
        assert(0);
        break;
    }

    if(i < e->comp_sz -1) fprintf(f,",");
  }
  fprintf(f,"]}");
}

static void _UnaToJSON( Parser* p , FILE* f , const NodeUnary* e ) {
  fprintf(f,"{ \"type\" : \"unary\" , \"op\" : \"%s\"" , e->op == TK_SUB ? "-" : "!");
  fprintf(f,",\"opr\" :");
  _NodeToJSON(p,f,e->opr);
  fprintf(f,"}");
}

static void _BinToJSON( Parser* p , FILE* f , const NodeBinary* e ) {
  fprintf(f,"{ \"type\" : \"binary\" , \"op\" : \"%s\"", TokenGetStr(e->op));

  {
    fprintf(f,",\"lhs\":");
    _NodeToJSON(p,f,e->lhs);
  }

  {
    fprintf(f,",\"rhs\":");
    _NodeToJSON(p,f,e->rhs);
  }

  fprintf(f,"}");
}

static void _TenToJSON( Parser*  p , FILE* f , const NodeTernary* e ) {
  fprintf(f,"{ \"type\" : \"ternary\" ,");

  {
    fprintf(f,"\"first\" :");
    _NodeToJSON(p,f,e->first);
  }

  {
    fprintf(f,",\"second\" :");
    _NodeToJSON(p,f,e->second);
  }

  {
    fprintf(f,",\"third\" :");
    _NodeToJSON(p,f,e->third);
  }

  fprintf(f,"}");
}


#ifdef CONFIG_UNITTEST
Node* ParseExpression( Parser* p ) {
  return ParseExpr(p);
}
#endif // CONFIG_UNITTEST
