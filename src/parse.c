#include "tcmm.h"

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <assert.h>

#define EXPR_MEMPOOL_SIZE 512

#define GRAB(XX) (MPoolGrab(&(p->pool),sizeof(XX)))
#define L(XX)    (&((XX)->lexer))
#define LEX(XX)  (&(L(XX)->lexeme))
#define NEXT(P)  LexerNext(L(P))

#define EXPECT(P,TK)                                                     \
  do {                                                                   \
    if((TK) != LEX(P)->tk) {                                             \
      ParserError((P),"expect token %s", TokenGetStr(L(P)->lexeme.tk));  \
    }                                                                    \
    NEXT(P);                                                             \
  } while(0)

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

PAPI void ParserInit( Parser* p , LitPool* lpool , TypeSys* tsys , const char* src ) {
  p->lpool = lpool;
  p->tsys  = tsys;
  LexerInit(&(p->lexer),lpool,src);
  MPoolInit(&(p->pool) ,1024 ,1024*64);
  p->err   = NULL;

  LexerNext(&(p->lexer)); // fire the lexer
}

PAPI void ParserDelete( Parser* p ) {
  LexerDelete(&(p->lexer));
  MPoolDelete(&(p->pool));
  free((void*)p->err);
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

  NodePrefixComp* c = NodePrefixAddComp(pp,&(p->pool));
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
          c            = NodePrefixAddComp(prefix,&(p->pool));
          c->comp_type = EEPCT_DOT;
          c->c.name    = LEX(p)->lit;
          NEXT(p);
          break;
        case TK_LSQR:
          NEXT(p); // skip [
          c = NodePrefixAddComp(prefix,&(p->pool));
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
