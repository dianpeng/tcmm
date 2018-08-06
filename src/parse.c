#include "tcmm.h"

#if 0

#include <stddef.h>
#include <stdint.h>

#define EXPR_MEMPOOL_SIZE 512

typedef struct _Parser {
  LitPool* lpool;
  TypeSys*   sys;

  Lexer    lexer;
  MPool     pool;
} Parser;

#define Grab(XX) (MPoolGrab(&(p->pool),sizeof(XX)))
#define L(XX) (&((XX)->lexer))
#define NEXT(P) LexerNext(L(P))

#define EXPECT(P,TK)                                                     \
  do {                                                                   \
    if((TK) != L(P)->lexeme.tk) {                                        \
      ParserError((P),"expect token %s", TokenGetStr(L(P)->lexeme.tk));  \
    }                                                                    \
    NEXT(P);                                                             \
  } while(0)

/** -------------------------------------
 * Expression Parsing
 * -------------------------------------*/
static Expr* ParseExpr   ( Parser* );
static Expr* ParseTernary( Parser* );
static Expr* ParseBinary ( Parser* );
static Expr* ParseUnary  ( Parser* );
static Expr* ParsePrefix ( Parser* );
static Expr* ParsePrimary( Parser* );
static Expr* ParseStruLit( Parser* );

static inline void ExprDbg0( Parser* p , Expr* n ) {
  n->nline     = L(p)->pos;
  n->nchar     = L(p)->nchar;
  n->dbg_start = L(p)->pos - L(p)->lexeme.tk_sz;
  n->dbg_end   = L(p)->pos;
}

static Expr* ParseStruLit( Parser* p ) {
}

static Expr* ParsePrimary( Parser* p ) {
  switch(L(p)->lexeme.tk) {
    case TK_LIT_INT:
    case TK_LIT_DBL:
    case TK_LIT_TRUE:
    case TK_LIT_FALSE:
    case TK_LIT_STR:
    case TK_LIT_CHAR:
      {
        ExprLit* n = Grab(ExprLit);
        n->base.type = EET_LIT;
        ExprDbg0(p,n);
        n->lit =  L(p)->lexeme.lit;
        NEXT(p);
        return n;
      }

    case TK_LPAR:
      {
        Expr* n;
        size_t dbg_start = L(p)->pos;

        NEXT(p);
        if(!(n = ParseExpr(p)))
          return NULL;
        EXPECT(p,TK_LPAR);

        // adjust debug info to include the "(" and ")"
        n->base.dbg_start = dbg_start;
        n->base.dbg_end   = L(p)->pos;
        return n;
      }

    case TK_ID:
      {
        ExprId* n = Grab(ExprId);
        n->base.type = EET_ID;
        ExprDbg0(n);
        n->lit = L(p)->lexeme.lit;
        NEXT(p);
        return n;
      }

    case TK_STRUCT:
      return ParseStruLit(p);

    default:
      ParserError(p,"expect a valid primary expression , eg literal , subexpression or struct literal");
      return NULL;
  }
}

#endif
