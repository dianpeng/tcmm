#include "tcmm.h"
#include <ctype.h>
#include <stdarg.h>
#include <assert.h>
#include <stdio.h>
#include <errno.h>

PAPI
const char* TokenGetStr( Token tk ) {
#define XX(A,B) case A: return B;
  switch(tk) {
    TOKEN_LIST(XX)
    default:
      return NULL;
  }
#undef XX // XX
}

PAPI
void LexerInit( Lexer* l , LitPool* pool , const char* source ) {
  l->src = source;
  l->pos = 0;
  l->nline = 1;
  l->nchar = 1;
  l->lpool = pool;
  l->lexeme.tk = TK_ERROR;
  l->err   = NULL;
}

typedef struct _Lookahead {
  int   c;
  Token t;
} Lookahead;

#define LH_END { 0 , TK_EOF }

static
const Lexeme* LexerError( Lexer* l , const char* fmt , ... ) {
  va_list vl;
  char   msg[1024];
  va_start(vl,fmt);
  vsnprintf(msg,1024,fmt,vl);
  l->err = ReportError("lexer",l->src,l->pos,l->nline,l->nchar,msg);
  l->lexeme.tk = TK_ERROR;
  l->lexeme.tk_sz = 0;
  return &(l->lexeme);
}

static
const Lexeme* LexerR ( Lexer* l , Token tk , size_t len ) {
  l->lexeme.tk    = tk;
  l->lexeme.tk_sz = len;
  l->pos   += len;
  l->nchar += len;
  return &(l->lexeme);
}

static
const Lexeme* LexerP( Lexer* l , Token t1 , Lookahead* lk ) {
  int c = l->src[l->pos+1];
  for( ; lk->t != TK_EOF ; ++lk ) {
    if(lk->c == c) {
      return LexerR(l,lk->t,2);
    }
  }
  return LexerR(l,t1,1);
}

static
const Lexeme* Lexer2( Lexer* l , int lc , Token tk ) {
  int c = l->src[l->pos+1];
  if(c == lc) {
    return LexerR(l,tk,2);
  }
  return LexerError(l,"expect char %c here to for token %s",lc,TokenGetStr(tk));
}

PAPI int LexerIsEscChar( int c ) {
  switch(c) {
    case 't':
    case 'v':
    case 'b':
    case 'n':
    case 'r':
    case '\'':
    case '"' :
    case '\\':
      return 1;
    default:
      return 0;
  }
}

PAPI int LexerEscChar( int c ) {
  switch(c) {
    case 't' : return '\t';
    case 'v' : return '\v';
    case 'b' : return '\b';
    case 'n' : return '\n';
    case 'r' : return '\r';
    case '\'': return '\'';
    case '"' : return '"' ;
    case '\\': return '\\';
    default:
      assert(0);
      return 0;
  }
}

static
const Lexeme* LexerNum( Lexer* l ) {
  int     off;
  int32_t val;

  if((off = StrToI32(l->src+l->pos,10,&val)) <0) {
    return LexerError(l,"integer number overflow");
  }

  // now check whether we can parse the whole stuff as floating point number
  if(l->src[l->pos+off] == '.') {
    double v;
    // errno can be set to ERANGE sometimes
    if((off=StrToDbl(l->src+l->pos,&v))<0) {
      return LexerError(l,"double number overflow");
    }

    l->lexeme.tk    = TK_LIT_DBL;
    l->lexeme.lit   = LitPoolGetDouble(l->lpool,v);
  } else {
    l->lexeme.tk    = TK_LIT_INT;
    l->lexeme.lit   = LitPoolGetInt(l->lpool,val);
  }

  l->lexeme.tk_sz = off;
  l->pos   += off;
  l->nchar += off;
  return &(l->lexeme);
}

static
const Lexeme* LexerChar( Lexer* l ) {
  size_t p = l->pos + 1; // skip '
  int    c = l->src[p];
  int    v = 0;

  if(c == '\\') {
    int nc = l->src[p+1];
    if(LexerIsEscChar(nc)) {
      ++p;
      v = LexerEscChar(nc);
    } else {
      return LexerError(l,"unknown escape character %c",nc);
    }
  } else if(c == '\'') {
    return LexerError(l,"character should be closed properly");
  } else {
    v = c;
  }

  ++p;
  if(l->src[p] != '\'') return LexerError(l,"expect \' to close char literal");
  ++p;

  l->lexeme.tk = TK_LIT_CHAR;
  l->lexeme.lit= LitPoolGetChar(l->lpool,v);
  l->lexeme.tk_sz = (p - l->pos);
  l->pos = p;
  l->nchar += l->lexeme.tk_sz;
  return &(l->lexeme);
}

static
const Lexeme* LexerStr( Lexer* l ) {
  size_t pos = l->pos + 1;
  int    c;
  char*  buf = malloc(1024);
  int    bp  = 0;
  int    bcap= 1024;
  char   pc  = 0;

  for( ; (c = l->src[pos]) ; pos++ ) {
    switch(c) {
      case '\\':
        if(LexerIsEscChar(l->src[pos+1])) {
          pc = LexerEscChar(l->src[pos+1]);
          pos++;
        } else {
          pc = '\\';
        }
        break;
      case '"':
        ++pos;
        goto done;
      case '\n':
      case '\t':
      case '\r':
      case '\v':
        goto fail_escp;
      default:
        pc = c;
        break;
    }

    if(bp == bcap) {
      buf   = realloc(buf,bcap*2);
      bcap *= 2;
    }
    buf[bp++] = pc;
  }

  free(buf);
  return LexerError(l,"string is not properly closed");

done:

  // append the last null terminator , note for the size checking
  if(bp == bcap)
    buf = realloc(buf,bcap+1);
  buf[bp]         = 0;

  l->lexeme.tk    = TK_LIT_STR;
  l->lexeme.lit   = LitPoolGetStr(l->lpool,buf);
  l->lexeme.tk_sz = (pos - l->pos);
  l->pos          = pos;
  l->nchar       += l->lexeme.tk_sz;

  free(buf);
  return &(l->lexeme);

fail_escp:
  free(buf);
  return LexerError(l,"contain character that needs to be escaped");
}

static
void LexerCmt( Lexer* l ) {
  const char* pos = strchr( l->src + l->pos , '\n' );
  if(pos == NULL) {
    size_t diff = strlen(l->src + l->pos);
    l->nchar += diff;
    l->pos   += diff;
  } else {
    l->pos    = (pos - l->src);
    l->nchar += (pos - (l->src + l->pos));
  }
}

static
int IsIdRChar( int c ) {
  return isdigit(c) || isalpha(c) || c == '_';
}

static
int IsIdIChar( int c ) {
  return isalpha(c) || c == '_';
}

static
int CmpKeyword( Lexer* l , const char* R ) {
  const char* start = l->src + l->pos;
  const char* L     = start + 1;
  for( ; *R ; ++R , ++L ) {
    if(*R != *L) return -1;
  }

  if(!IsIdRChar(*L)) {
    return (L-start);
  } else {
    return -1;
  }
}

static
const Lexeme* LexerId( Lexer* l ) {
  size_t pos = l->pos+1;
  int      c = l->src[pos];
  while(IsIdRChar(c)) {
    c = l->src[++pos];
  }
  l->lexeme.tk = TK_ID;
  l->lexeme.lit= LitPoolGetId(l->lpool,l->src + l->pos,l->src+pos);
  l->lexeme.tk_sz = (pos - l->pos);
  l->pos = pos;
  l->nchar += l->lexeme.tk_sz;
  return &(l->lexeme);
}

static
const Lexeme* LexerKeywordOrId( Lexer* l ) {
  int c = l->src[l->pos];
  int r;
  switch(c) {
    case 'b':
      if((r = CmpKeyword(l,"ool")) >0)
        return LexerR(l,TK_BOOL,r);
      else if((r = CmpKeyword(l,"reak")) >0)
        return LexerR(l,TK_BREAK,r);
      goto id;
    case 'c':
      if((r = CmpKeyword(l,"har")) >0)
        return LexerR(l,TK_CHAR,r);
      else if((r = CmpKeyword(l,"ontinue")) >0)
        return LexerR(l,TK_CONTINUE,r);
      goto id;
    case 'd':
      if((r = CmpKeyword(l,"ouble")) >0)
        return LexerR(l,TK_DBL,r);
      goto id;
    case 'e':
      if((r = CmpKeyword(l,"lse")) >0)
        return LexerR(l,TK_ELSE,r);
      else if((r = CmpKeyword(l,"lif")) >0)
        return LexerR(l,TK_ELIF,r);
      goto id;
    case 'f':
      if((r = CmpKeyword(l,"alse")) >0)
        return LexerR(l,TK_LIT_FALSE,r);
      else if((r = CmpKeyword(l,"or")) >0)
        return LexerR(l,TK_FOR,r);
      goto id;
    case 'i':
      if((r = CmpKeyword(l,"f")) >0)
        return LexerR(l,TK_IF,r);
      else if((r = CmpKeyword(l,"nt")) >0)
        return LexerR(l,TK_INT,r);
      goto id;
    case 'r':
      if((r = CmpKeyword(l,"eturn")) >0)
        return LexerR(l,TK_RETURN,r);
      goto id;
    case 's':
      if((r = CmpKeyword(l,"tring")) >0)
        return LexerR(l,TK_STR,r);
      else if((r = CmpKeyword(l,"truct")) >0)
        return LexerR(l,TK_STRUCT,r);
      goto id;
    case 't':
      if((r = CmpKeyword(l,"rue")) >0)
        return LexerR(l,TK_LIT_TRUE,r);
      goto id;
    case 'v':
      if((r = CmpKeyword(l,"oid")) >0)
        return LexerR(l,TK_VOID,r);
      goto id;
    default:
      if(IsIdIChar(c)) {
id:
        return LexerId(l);
      }
      break;
  }

  return LexerError(l,"unrecognized character %c here",c);
}

PAPI
const Lexeme* LexerNext( Lexer* l ) {
  for( ;; ) {
    int c = l->src[l->pos];
    switch(c) {
      case '+' : return LexerP(l,TK_ADD, (Lookahead[]){{'+',TK_INC},{'=',TK_SADD},LH_END});
      case '-' : return LexerP(l,TK_SUB, (Lookahead[]){{'-',TK_DEC},{'=',TK_SSUB},LH_END});
      case '*' : return LexerP(l,TK_MUL, (Lookahead[]){{'=',TK_SMUL},LH_END});
      case '/' : return LexerP(l,TK_DIV, (Lookahead[]){{'=',TK_SDIV},LH_END});
      case '%' : return LexerP(l,TK_MOD, (Lookahead[]){{'=',TK_SMOD},LH_END});
      case '<' : return LexerP(l,TK_LT , (Lookahead[]){{'=',TK_LE},LH_END});
      case '>' : return LexerP(l,TK_GT , (Lookahead[]){{'=',TK_GE},LH_END});
      case '=' : return LexerP(l,TK_ASSIGN,(Lookahead[]){{'=',TK_EQ},LH_END});
      case '&' : return LexerP(l,TK_ADDR,(Lookahead[]){{'&',TK_AND},LH_END});
      case '|' : return Lexer2(l,'|',TK_OR);
      case '.' : return LexerR(l,TK_DOT,1);
      case ':' : return LexerR(l,TK_COLON,1);
      case ';' : return LexerR(l,TK_SEMICOLON,1);
      case '?' : return LexerR(l,TK_QUESTION,1);
      case ',' : return LexerR(l,TK_COMMA,1);

      case '(' : return LexerR(l,TK_LPAR,1);
      case ')' : return LexerR(l,TK_RPAR,1);
      case '[' : return LexerR(l,TK_LSQR,1);
      case ']' : return LexerR(l,TK_RSQR,1);
      case '{' : return LexerR(l,TK_LBRA,1);
      case '}' : return LexerR(l,TK_RBRA,1);

      case '!' : return LexerP(l,TK_NOT,(Lookahead[]){{'=',TK_NE},LH_END});

      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
        return LexerNum(l);

      case '\'':
        return LexerChar(l);

      case '"':
        return LexerStr (l);

      case '#':
        LexerCmt (l);
        break;

      // whitespaces handling
      case ' ': case '\t': case '\v': case '\b': case '\r':
        ++l->pos;
        ++l->nchar;
        break;

      case '\n':
        ++l->pos;
        ++l->nline;
        l->nchar = 0;
        break;

      case 0:
        return LexerR(l,TK_EOF,0);

      default:
        return LexerKeywordOrId(l);
    }
  }
}

PAPI
void LexerDelete( Lexer* l ) {
  free((void*)l->err);
}
