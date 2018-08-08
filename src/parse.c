#include "tcmm.h"

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>

typedef struct _Parser {
  LitPool* lpool;
  Lexer    lexer;
  MPool*    pool;
  const char* err;
  int      lp_cnt;

	NodeFunc* nf; // currently parsing NodeFunc node
} Parser;

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

static void ParserInit( Parser* p , LitPool* lpool , MPool* mpool , const char* src ) {
  p->lpool = lpool;
  p->pool  = mpool;
	p->lp_cnt= 0;
  p->err   = NULL;

  LexerInit(&(p->lexer),lpool,src);
  LexerNext(&(p->lexer)); // fire the lexer
}

static void ParserDelete( Parser* p ) {
  LexerDelete(&(p->lexer));
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

static NodePrefixComp* NodePrefixAddComp( NodePrefix* p , MPool* pool ) {
  if(p->comp_sz == p->comp_cap) {
    size_t ncap = p->comp_cap ? p->comp_cap * 2 : 4;
    p->comp = MPoolRealloc(pool,p->comp,sizeof(NodePrefixComp)*p->comp_cap,ncap);
    p->comp_cap = ncap;
  }

  return p->comp + p->comp_sz++;
}


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
  size_t pos_start = L(p)->pos;

  assert( LEX(p)->tk == TK_STRUCT );
  NEXT(p);

  // get the type object w.r.t the type name
  if(LEX(p)->tk != TK_ID) {
    ParserError(p,"expect a ID to indicate type name");
    return NULL;
  }
  NEXT(p); // skip the type name
  EXPECT(p,TK_LBRA); // expect a {

  // create the NodeStruLit object
  slit = GRAB(NodeStruLit);
  slit->base.type      = ENT_STRULIT;
  slit->base.dbg_start = pos_start;

  slit->ctype  = NULL;
  slit->assign = NULL;

  // parsing struct literal field
  if(LEX(p)->tk == TK_RBRA) {
    NEXT(p);
  } else {
    do {
      NodeStruLitAssign* field = NULL;

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
      field->ctype= NULL;
      field->name = LEX(p)->lit;
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
        n->ctype = NULL;
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
        EXPECT(p,TK_RPAR);

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
    prefix->ctype          = NULL;

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
    una->ctype = NULL;
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
        bin->ctype          = NULL;
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
    ten->ctype = NULL;

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
static int ParseVDec( Parser* , TypeInfo* , LitIdx* ref );

static CodeChunk* ParseChunk       ( Parser* );
static CodeChunk* ParseChunkOrSStmt( Parser* );

static Node* ParseLocal       ( Parser* );
static Node* ParseCallOrAssign( Parser* , Token );
static Node* ParseIf          ( Parser* );
static Node* ParseFor         ( Parser* );
static Node* ParseContinue    ( Parser* );
static Node* ParseBreak       ( Parser* );
static Node* ParseReturn      ( Parser* );
static Node* ParseStmt        ( Parser* );

static void CodeChunkAddStmt( Parser* p , CodeChunk* ck , Node* stmt ) {
  if(ck->cap == ck->sz) {
    size_t ncap = ck->cap ? ck->cap * 2 : 16;
    ck->stmt    = MPoolRealloc(p->pool,ck->stmt,sizeof(Node*)*ck->sz,sizeof(Node*)*ncap);
    ck->cap     = ncap;
  }
  ck->stmt[ck->sz++] = stmt;
}

static int ParseVDec( Parser* p , TypeInfo* tinfo , LitIdx* ref ) {
  switch(LEX(p)->tk) {
    case TK_INT:
      tinfo->type = EPT_INT;
      break;
    case TK_DBL:
      tinfo->type = EPT_DBL;
      break;
    case TK_BOOL:
      tinfo->type = EPT_BOOL;
      break;
    case TK_CHAR:
      tinfo->type = EPT_CHAR;
      break;
    case TK_STR:
      tinfo->type = ET_STR;
      break;
    case TK_STRUCT:
      {
        NEXT(p);
        if(LEX(p)->tk != TK_ID) {
          ParserError(p,"expect a ID name after the struct");
          return -1;
        }
        tinfo->type = ET_STRUCT;
        tinfo->extra.name = LEX(p)->lit;
      }
      break;
    case TK_VOID:
      ParserError(p,"void type cannot be used for declare variable");
      return -1;
    default:
      ParserError(p,"unknown token here, expect a type specifier");
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

    tinfo->extra.arr.t   = tinfo->type;
    tinfo->type          = ET_ARR;
    tinfo->extra.arr.len = len;

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
  LitIdx      vid;
  size_t      dbg_start = L(p)->pos;
  NodeLocal*  l;

  l = GRAB(NodeLocal);

  if(ParseVDec(p,&(l->tinfo),&vid)) return NULL;


  l->base.type      = ENT_LOCAL;
  l->base.dbg_start = dbg_start;
  l->base.dbg_end   = L(p)->pos;
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

static Node* ParseCallOrAssign( Parser* p , Token delim ) {
  size_t dbg_start = L(p)->pos;

  Node* lhs = ParseExpr(p); // try to parse if as a lhs expression
  if(!lhs) return NULL;

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
    EXPECT(p,delim);

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
        EXPECT(p,delim);
        return lhs;
      }
    }
  }

  ParserError(p,"meaningless statement, expect a call or a assignment");
  return NULL;
}

static int ParseBranch( Parser* p , NodeIf* n , int is_else ) {
  Node* cond = NULL;
  NodeIfBranch* if_br;

  if(!is_else) {
    // try to parse the condition
    if(LEX(p)->tk == TK_LPAR) {
      NEXT(p);
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
  if(!(if_br->chunk = ParseChunkOrSStmt(p)))
    return -1;
  if_br->cond = cond;

  return 0;
}

static Node* ParseIf( Parser* p ) {
  size_t dbg_start = L(p)->pos;
  NodeIf* n        = GRAB(NodeIf);

  n->base.type = ENT_IF;
  n->base.dbg_start = dbg_start;

  n->chains = NULL;
  n->sz     = 0;
  n->cap    = 0;

  // parse the leading if
  NEXT(p); // skip if
  if(ParseBranch(p,n,0))
    return NULL;

  // elif cluster
  while(LEX(p)->tk == TK_ELIF) {
    NEXT(p);
    if(ParseBranch(p,n,0)) return NULL;
  }

  // dangling else
  if(LEX(p)->tk == TK_ELSE) {
    NEXT(p);
    if(ParseBranch(p,n,1)) return NULL;
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
      return ParseCallOrAssign(p,TK_SEMICOLON);
  }
}

static Node* ParseFor( Parser* p ) {
  size_t  dbg_start = L(p)->pos;
  NodeFor* ret;

  Node* init = NULL;
  Node* cond = NULL;
  Node* step = NULL;
  CodeChunk* ck = NULL;

  NEXT(p); EXPECT(p,TK_LPAR);

  // setup a new lexical scope
	++p->lp_cnt;

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
    if(!(step = ParseCallOrAssign(p,TK_RPAR))) return NULL;
  }

  if(!(ck = ParseChunkOrSStmt(p)))
    return NULL;

  ret = GRAB(NodeFor);
  ret->base.type = ENT_FOR;
  ret->base.dbg_start = dbg_start;
  ret->base.dbg_end   = L(p)->pos;

  ret->init = init;
  ret->cond = cond;
  ret->step = step;
  ret->chunk= ck;

	--p->lp_cnt;

  return (Node*)ret;
}

static Node* ParseContinue( Parser* p ) {
  NodeContinue* c;
  if(p->lp_cnt == 0) {
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
  if(p->lp_cnt == 0) {
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

  NEXT(p); // skip return

  if(LEX(p)->tk != TK_SEMICOLON) {
    if(!(rv = ParseExpr(p)))
      return NULL;
  }

  EXPECT(p,TK_SEMICOLON);
  r = GRAB(NodeReturn);

  r->base.type      = ENT_RETURN;
  r->base.dbg_start = dbg_start;
  r->base.dbg_end   = L(p)->pos;
  r->expr           = rv;

  return (Node*)r;
}

static CodeChunk* ParseChunk( Parser* p ) {
  CodeChunk* ret;

  assert(LEX(p)->tk == TK_LBRA);
  NEXT(p);

  ret = GRAB(CodeChunk);
  ret->stmt = NULL;
  ret->sz   = 0;
  ret->cap  = 0;

  if(LEX(p)->tk != TK_RBRA) {
    for( ;; ) {
      Node* stmt = ParseStmt(p);
      if(!stmt) return NULL;
      CodeChunkAddStmt(p,ret,stmt);
      if(LEX(p)->tk == TK_RBRA) {
        break;
      }
    }
  }

  NEXT(p); // skip the }

  return ret;
}

static CodeChunk* ParseChunkOrSStmt( Parser* p ) {
  if(LEX(p)->tk == TK_LBRA) {
    return ParseChunk(p);
  } else {
    CodeChunk* ret = GRAB(CodeChunk);
    Node* stmt;

    if(!(stmt = ParseStmt(p)))
        return NULL;

    ret->stmt = MPoolGrab(p->pool,sizeof(Node*));
    ret->stmt[0] = stmt;
    ret->cap     = 1;
    ret->sz      = 1;

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
      return ParseCallOrAssign(p,TK_SEMICOLON);
  }
}

static int ParseFuncArgList( Parser* p , NodeFunc* nf ) {
  NEXT(p); // skip (

  if(LEX(p)->tk == TK_RPAR) {
    NEXT(p);
  } else {
    for( ;; ) {
      NodeFuncArgDef* def;
      if(nf->arg_sz == nf->arg_cap) {
        size_t ncap = nf->arg_cap ? nf->arg_cap * 2 : 4;
        nf->arg     = MPoolRealloc(p->pool,nf->arg,sizeof(NodeFuncArgDef)*nf->arg_sz,
                                                   sizeof(NodeFuncArgDef)*ncap);
        nf->arg_cap = ncap;
      }

      def = nf->arg + nf->arg_sz++;

      if(nf->arg_sz == CONFIG_MAX_CALL_ARGS) {
        ParserError(p,"too many function arguments, only allowed %d",CONFIG_MAX_CALL_ARGS);
        return -1;
      }

      if(ParseVDec(p,&(def->tinfo),&(def->name))) return -1;

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

static Node* ParseFunc( Parser* p , size_t dbg_start , LitIdx fname , const TypeInfo* tinfo ) {
  assert(LEX(p)->tk == TK_LPAR);
  {
    NodeFunc* fnode = GRAB(NodeFunc);

    fnode                 = GRAB(NodeFunc);
    fnode->base.type      = ENT_FUNC;
    fnode->base.dbg_start = dbg_start;
    fnode->ctype          = NULL;

    fnode->arg            = NULL;
    fnode->arg_sz         = 0;
    fnode->arg_cap        = 0;
    fnode->rtype          = *tinfo;

    if(ParseFuncArgList(p,fnode)) return NULL;

    if(LEX(p)->tk != TK_LBRA) {
      ParserError(p,"expect a \"{\" to start a function body");
      return NULL;
    }

    if(!(fnode->chunk = ParseChunk(p)))
      return NULL;

    fnode->base.dbg_end   = L(p)->pos;
    return (Node*)fnode;
  }
}

static Node* ParseGlbVar( Parser* p , size_t dbg_start , LitIdx name , const TypeInfo* type ) {
  NodeGlobal* ret;
  Node*       val;
  NEXT(p); // skip the =
  if(!(val = ParseExpr(p))) return NULL;
  EXPECT(p,TK_SEMICOLON);

  ret = GRAB(NodeGlobal);
  ret->base.type      = ENT_GLOBAL;
  ret->base.dbg_start = dbg_start;
  ret->base.dbg_end   = L(p)->pos;
  ret->value          = val;
  ret->tinfo          = *type;
  ret->ctype          = NULL;

  return (Node*)ret;
}

static Node* ParseGlbVarOrFunction( Parser* p ) {
  size_t      dbg_start = L(p)->pos;
  LitIdx      name;
  TypeInfo    t;

  if(ParseVDec(p,&t,&name)) return NULL;

  if(LEX(p)->tk == TK_ASSIGN) {
    return ParseGlbVar(p,dbg_start,name,&t);
  } else if(LEX(p)->tk == TK_LPAR) {
    // check whether it can be a function or not
    if(t.type == ET_ARR) {
      ParserError(p,"doesn't look like a function definition , neither looks like a global variable");
      return NULL;
    }
    return ParseFunc(p,dbg_start,name,&t);
  }

  ParserError(p,"unknown global statement, you can define struct type, global variables or function "
                "in global scope");
  return NULL;
}

static Node* ParseStruct( Parser* p ) {
  NodeStructDef* def = GRAB(NodeStructDef);

  def->base.type      = ENT_STRUCT_DEF;
  def->base.dbg_start = L(p)->pos;

  def->field = NULL;
  def->sz    = 0;
  def->cap   = 0;

  NEXT(p); // skip "struct"
  if(LEX(p)->tk != TK_ID) {
    ParserError(p,"expect a id after the struct");
    return NULL;
  }

  def->name = LEX(p)->lit;
  NEXT(p); // skip the id

  if(LEX(p)->tk != TK_LBRA) {
    ParserError(p,"expect a \"{\" to start the struct body");
    return NULL;
  }
  NEXT(p);
  while(LEX(p)->tk != TK_RBRA) {
    NodeStructDefField* field;

    if(def->sz == def->cap) {
      size_t ncap = def->cap ? def->cap * 2 : 4;
      def->field  = MPoolRealloc(p->pool,def->field,sizeof(NodeStructDefField)*def->sz,
                                                    sizeof(NodeStructDefField)*ncap);
    }

    field = def->field + def->sz++;

    if(ParseVDec(p,&(field->tinfo),&(field->name)))
      return NULL;

    if(LEX(p)->tk == TK_SEMICOLON) {
      NEXT(p);
    } else {
      ParserError(p,"expect a \";\" or \"}\" in struct body");
      return NULL;
    }
  }

  NEXT(p); // skip the last }
  EXPECT(p,TK_SEMICOLON);

  def->base.dbg_end = L(p)->pos;
  return (Node*)def;
}

static NodeFile* ParseFile( Parser* p ) {
  Node* stmt;
  NodeFile* all       = GRAB(NodeFile);
  all->base.type      = ENT_FILE;
  all->base.dbg_start = L(p)->pos;
  all->chunk          = GRAB(CodeChunk);
  all->chunk->stmt    = NULL;
  all->chunk->sz      = 0;
  all->chunk->cap     = 0;

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
        if(!(stmt=ParseStruct(p)))
          return NULL;
        break;
      case TK_EOF:
        goto done;
      default:
        ParserError(p,"unknown global scope statement");
        return NULL;
    }
    CodeChunkAddStmt(p,all->chunk,stmt);
  }

done:
  assert(p->lp_cnt == 0);
  return all;
}

PAPI NodeFile* Parse( LitPool* lpool , MPool* mpool , const char* src ) {
  NodeFile* ret;
  Parser p;

  ParserInit(&p,lpool,mpool,src);
  ret = ParseFile(&p);
  ParserDelete(&p);

  return ret;
}

/** ----------------------------------------------------------------
 * ToJSON
 *
 *  JSON is good for us to do unittesting, since all we need to do is
 *  having a JSON parser. Another good format will be sexpression , but
 *  using JSON is good enough for us to do automatic parsing testing.
 *
 * ----------------------------------------------------------------*/
static void _NodeToJSON    ( Parser* , FILE* , const Node* );
static void _LitToJSON     ( Parser* , FILE* , const NodeLit*    );
static void _IdToJSON      ( Parser* , FILE* , const NodeId*     );
static void _SLitToJSON    ( Parser* , FILE* , const NodeStruLit*);
static void _PrefixToJSON  ( Parser* , FILE* , const NodePrefix* );
static void _UnaToJSON     ( Parser* , FILE* , const NodeUnary*  );
static void _BinToJSON     ( Parser* , FILE* , const NodeBinary* );
static void _TenToJSON     ( Parser* , FILE* , const NodeTernary* );
static void _LocalToJSON   ( Parser* , FILE* , const NodeLocal*   );
static void _AssignToJSON  ( Parser* , FILE* , const NodeAssign*  );
static void _IfToJSON      ( Parser* , FILE* , const NodeIf*      );
static void _ForToJSON     ( Parser* , FILE* , const NodeFor*     );
static void _ContinueToJSON( Parser* , FILE* , const NodeContinue* );
static void _BreakToJSON   ( Parser* , FILE* , const NodeBreak*    );
static void _ReturnToJSON  ( Parser* , FILE* , const NodeReturn*  );
static void _FuncToJSON    ( Parser* , FILE* , const NodeFunc*    );
static void _GlobalToJSON  ( Parser* , FILE* , const NodeGlobal*  );
static void _StructToJSON  ( Parser* , FILE* , const NodeStructDef* );
static void _FileToJSON    ( Parser* , FILE* , const NodeFile*    );

// Dump the expression into JSON format for better visualization or
// unittest purpose since JSON is a universally parsable format and
// it is easy to hook it into other toolings
void NodeToJSON( LitPool* lpool , MPool* mpool , FILE* output , const Node* e ) {
  Parser p;
  p.lpool = lpool;
  p.pool  = mpool;

  _NodeToJSON(&p,output,e);
}

static void _NodeToJSON( Parser* p, FILE* f, const Node* e ) {
  switch(e->type) {
    case ENT_LIT:       _LitToJSON(p,f,(const NodeLit*)e);       break;
    case ENT_ID:        _IdToJSON (p,f,(const NodeId* )e);       break;
    case ENT_STRULIT:   _SLitToJSON(p,f,(const NodeStruLit*)e);  break;
    case ENT_PREFIX:    _PrefixToJSON(p,f,(const NodePrefix*)e); break;
    case ENT_UNARY:     _UnaToJSON(p,f,(const NodeUnary*)e);     break;
    case ENT_BINARY:    _BinToJSON(p,f,(const NodeBinary*)e);    break;
    case ENT_TERNARY:   _TenToJSON(p,f,(const NodeTernary*)e);   break;
    case ENT_LOCAL:     _LocalToJSON(p,f,(const NodeLocal*)e);   break;
    case ENT_ASSIGN:    _AssignToJSON(p,f,(const NodeAssign*)e); break;
    case ENT_IF:        _IfToJSON(p,f,(const NodeIf*)e);         break;
    case ENT_FOR:       _ForToJSON(p,f,(const NodeFor*)e);       break;
    case ENT_CONTINUE:  _ContinueToJSON(p,f,(const NodeContinue*)e); break;
    case ENT_BREAK:     _BreakToJSON(p,f,(const NodeBreak*)e); break;
    case ENT_RETURN:    _ReturnToJSON(p,f,(const NodeReturn*)e); break;
    case ENT_FUNC:      _FuncToJSON(p,f,(const NodeFunc*)e);     break;
    case ENT_STRUCT_DEF:_StructToJSON(p,f,(const NodeStructDef*)e); break;
    case ENT_GLOBAL:    _GlobalToJSON(p,f,(const NodeGlobal*)e); break;
    case ENT_FILE:      _FileToJSON(p,f,(const NodeFile*)e);     break;
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

static void _PrintPrimType( FILE* f , EType t ) {
  switch(t) {
    case ET_STR: fprintf(f,"str"); break;
    case EPT_INT: fprintf(f,"int"); break;
    case EPT_DBL: fprintf(f,"dbl"); break;
    case EPT_CHAR: fprintf(f,"char"); break;
    case EPT_BOOL: fprintf(f,"bool"); break;
    case EPT_VOID: fprintf(f,"void"); break;
    default: break;
  }
}

static void _PrintRawType( Parser* p , FILE* f , const TypeInfo* t ) {
  switch(t->type) {
    case ET_ARR:
      _PrintPrimType(f,t->extra.arr.t);
      fprintf(f,"[%d]",(int)t->extra.arr.len);
      break;
    case ET_STRUCT:
      fprintf(f,"struct %s",LitPoolId(p->lpool,t->extra.name));
      break;
    case ET_FUNC:
      fprintf(f,"func %s",LitPoolId(p->lpool,t->extra.name));
      break;
    default:
      _PrintPrimType(f,t->type);
      break;
  }
}

static void _PrintType( Parser* p , FILE* f , const TypeInfo* tinfo ) {
  fprintf(f,"\""); _PrintRawType(p,f,tinfo); fprintf(f,"\"");
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
  fprintf(f,"{ \"type\" : \"struct-literal\" , \"name\" : \"%s\" , value : [ ",LitPoolId(p->lpool,slit->ctype->name));
  {
    NodeStruLitAssign* field = slit->assign;
    for( ; field ; field = field->next ) {
      fprintf(f,"{ \"name\" : \"%s\"" , LitPoolId(p->lpool,field->name));
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

/** statement dump **/
static void _CodeChunkToJSON( Parser* p , FILE* f , const CodeChunk* cc ) {
  fprintf(f,"[");
  for(size_t i = 0 ; i < cc->sz ; ++i) {
    _NodeToJSON(p,f,cc->stmt[i]);
    if(i < cc->sz-1) fprintf(f,",");
  }
  fprintf(f,"]");
}

static void _LocalToJSON( Parser* p , FILE* f , const NodeLocal* e ) {
  fprintf(f,"{ \"type\" : \"local\" , \"ctype\" : ");
  _PrintType(p,f,&(e->tinfo));
  fprintf(f,", \"name\" : \"%s\"" ,LitPoolId(p->lpool,e->name));
  fprintf(f,", \"rhs\"  : ");
  _NodeToJSON(p,f,e->rhs);
  fprintf(f,"}");
}

static void _AssignToJSON( Parser* p ,FILE* f , const NodeAssign* e ) {
  fprintf(f,"{ \"type\" : \"assign\" , \"lhs\" : ");
  _NodeToJSON(p,f,e->lhs);

  fprintf(f,",\"rhs\" :");
  _NodeToJSON(p,f,e->rhs);

  fprintf(f,",\"op\" : \"%s\"}",TokenGetStr(e->op));
}

static void _IfToJSON( Parser* p , FILE* f , const NodeIf* e ) {
  fprintf(f,"{ \"type\" : \"if\" , \"chain\" : [");
  for( size_t i = 0 ; i < e->sz ; ++i ) {
    NodeIfBranch* br = e->chains + i;
    fprintf(f,"{ \"cond\" : ");
    if(br->cond) {
      _NodeToJSON(p,f,br->cond);
    } else {
      fprintf(f,"null");
    }

    fprintf(f,", \"body\":");
    _CodeChunkToJSON(p,f,br->chunk);
    if(i < e->sz-1) {
      fprintf(f,"},");
    } else {
      fprintf(f,"}");
    }
  }

  fprintf(f,"]}\n");
}

static void _ForToJSON( Parser* p , FILE* f , const NodeFor* e ) {
  fprintf(f,"{ \"type\" : \"for\" , ");
  {
    fprintf(f,"\"init\" : ");
    if(e->init) {
      _NodeToJSON(p,f,e->init);
    } else {
      fprintf(f,"null");
    }
  }
  {
    fprintf(f,", \"cond\" : ");
    if(e->cond) {
      _NodeToJSON(p,f,e->cond);
    } else {
      fprintf(f,"null");
    }
  }
  {
    fprintf(f,", \"step\" : ");
    if(e->step) {
      _NodeToJSON(p,f,e->step);
    } else {
      fprintf(f,"null");
    }
  }

  fprintf(f,",\"body\" :");
  _CodeChunkToJSON(p,f,e->chunk);
  fprintf(f,"}");
}

static void _BreakToJSON( Parser* p , FILE* f , const NodeBreak* e ) {
  fprintf(f,"{ \"type\" : \"break\" }");
}

static void _ContinueToJSON( Parser* p , FILE* f , const NodeContinue* e ) {
  fprintf(f,"{ \"type\" : \"continue\" }");
}

static void _ReturnToJSON( Parser* p , FILE* f , const NodeReturn* e ) {
  fprintf(f,"{ \"type\" : \"return\" , \"value\" : ");
  if(e->expr) {
    _NodeToJSON(p,f,e->expr);
  } else {
    fprintf(f,"null");
  }

  fprintf(f,"}");
}

static void _FuncToJSON( Parser* p , FILE* f , const NodeFunc* e ) {
  fprintf(f,"{ \"type\" : \"func\" , \"return\" : ");
  _PrintType(p,f,&(e->rtype));
  fprintf(f,", \"arg\" : [");
  for( size_t i = 0 ; i < e->arg_sz ; ++i ) {
    NodeFuncArgDef* def = e->arg + i;
    fprintf(f,"{ \"name\" : \"%s\" ",LitPoolId(p->lpool,def->name));
    fprintf(f,", \"type\" : ");
    _PrintType(p,f,&(def->tinfo));
    fprintf(f,"}");
    if(i < e->arg_sz - 1)
      fprintf(f,",");
  }
  fprintf(f,"], \"body\" :");
  _CodeChunkToJSON(p,f,e->chunk);
  fprintf(f,"}");
}

static void _GlobalToJSON( Parser* p ,FILE* f , const NodeGlobal* e ) {
  fprintf(f,"{ \"type\" : \"global\" , \"ctype\" :");
  _PrintType(p,f,&(e->tinfo));
  fprintf(f,",\"value\" :");
  _NodeToJSON(p,f,e->value);
  fprintf(f,"}");
}

static void _StructToJSON( Parser* p , FILE* f , const NodeStructDef* e ) {
  fprintf(f,"{ \"type\" : \"struct\" , \"name\" : \"%s\" ",LitPoolId(p->lpool,e->name));
  fprintf(f,", \"field\": [");
  for( size_t i = 0 ; i < e->sz ; ++i ) {
    NodeStructDefField* fe = e->field + i;
    fprintf(f,"{ \"name\" : \"%s\" ",LitPoolId(p->lpool,fe->name));
    fprintf(f,", \"type\" : ");
    _PrintType(p,f,&(fe->tinfo));
    fprintf(f,"}");
    if(i < e->sz-1) fprintf(f,",");
  }
  fprintf(f,"]}");
}

static void _FileToJSON( Parser* p , FILE* f , const NodeFile* e ) {
  _CodeChunkToJSON(p,f,e->chunk);
}

#ifdef CONFIG_UNITTEST
Node* ParseExpression( LitPool* lpool , MPool* pool , const char* source ) {
  Node* ret;
  Parser p;
  ParserInit(&p,lpool,pool,source);
  ret = ParseExpr(&p);
  ParserDelete(&p);
  return ret;
}
#endif // CONFIG_UNITTEST
