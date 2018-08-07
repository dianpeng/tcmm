#include "../src/tcmm.h"
#include <cunitpp.h>

#define array_size(ARR) (sizeof(ARR)/sizeof((ARR)[0]))

MPool* mpool;

TEST(Lexer,TkOp) {
  {
    const char* str = "+-*/% ++ -- < <= > >= != ! = == && || . ! , ; ? : &";
    Token tk[]      = {
      TK_ADD,
      TK_SUB,
      TK_MUL,
      TK_DIV,
      TK_MOD,
      TK_INC,
      TK_DEC,
      TK_LT ,
      TK_LE ,
      TK_GT ,
      TK_GE ,
      TK_NE ,
      TK_NOT,
      TK_ASSIGN,
      TK_EQ ,
      TK_AND,
      TK_OR ,
      TK_DOT,
      TK_NOT,
      TK_COMMA,
      TK_SEMICOLON,
      TK_QUESTION ,
      TK_COLON,
      TK_ADDR,
      TK_EOF
    };

    Lexer l;
    LitPool lpool;

    LitPoolInit(&lpool,mpool);
    LexerInit  (&l,&lpool,str);

    for( size_t i = 0 ; i < array_size(tk) ; ++i ) {
      ASSERT_EQ(tk[i],LexerNext(&l)->tk);
    }

    LitPoolDelete(&lpool);
    LexerDelete  (&l);
  }

  {
    const char* str = "{ } [ ] ( ) += -= *= /= %=";
    Token tk[] = {
      TK_LBRA,
      TK_RBRA,
      TK_LSQR,
      TK_RSQR,
      TK_LPAR,
      TK_RPAR,
      TK_SADD,
      TK_SSUB,
      TK_SMUL,
      TK_SDIV,
      TK_SMOD,
      TK_EOF
    };
    Lexer l;
    LitPool lpool;

    LitPoolInit(&lpool,mpool);
    LexerInit  (&l,&lpool,str);

    for( size_t i = 0 ; i < array_size(tk) ; ++i ) {
      ASSERT_EQ(tk[i],LexerNext(&l)->tk);
    }

    LitPoolDelete(&lpool);
    LexerDelete  (&l);
  }
}

typedef union _Value {
  int ival;
  double rval;
  int bval;
  char cval;
  const char* sval;
} Value;

typedef struct _Result {
  Value v;
  Token tk;
} Result;

TEST(Lexer,NumNormal) {
  const char* str = "123 123. 123.1 1";
  Result r[] = {
    { .v.ival = 123   , .tk = TK_LIT_INT },
    { .v.rval = 123.0 , .tk = TK_LIT_DBL },
    { .v.rval = 123.1 , .tk = TK_LIT_DBL },
    { .v.ival = 1     , .tk = TK_LIT_INT },
    { .tk = TK_EOF }
  };

  Lexer l;
  LitPool lpool;

  LitPoolInit(&lpool,mpool);
  LexerInit  (&l,&lpool,str);

  for( size_t i = 0 ; i < array_size(r) ; ++i ) {
    ASSERT_EQ(r[i].tk,LexerNext(&l)->tk);
    const Lit* lit = LitPoolIndex(&lpool,l.lexeme.lit);
    if(r[i].tk == TK_LIT_INT) {
      ASSERT_EQ(ELT_INT,lit->type);
      ASSERT_EQ(lit->d.ival,r[i].v.ival);
    } else if(r[i].tk == TK_LIT_DBL) {
      ASSERT_EQ(ELT_DBL,lit->type);
      ASSERT_EQ(lit->d.rval,r[i].v.rval);
    }
  }

  LitPoolDelete(&lpool);
  LexerDelete  (&l);
}

TEST(Lexer,Int32Overflow) {
  // try to test overflow
  const char* str = "1231122334455667788990011223344556677889900112323421313213123";
  Lexer l;
  LitPool lpool;

  LitPoolInit(&lpool,mpool);
  LexerInit  (&l,&lpool,str);
  LexerNext(&l);
  ASSERT_EQ(TK_ERROR,l.lexeme.tk);

  LitPoolDelete(&lpool);
  LexerDelete  (&l);
}

TEST(Lexer,Boolean) {
  const char* str = "true false";
  LitPool lpool;
  Lexer l;

  LitPoolInit(&lpool,mpool);
  LexerInit  (&l,&lpool,str);

  LexerNext(&l);
  ASSERT_EQ(TK_LIT_TRUE,l.lexeme.tk);
  ASSERT_EQ(TK_LIT_FALSE,LexerNext(&l)->tk);
  ASSERT_EQ(TK_EOF,LexerNext(&l)->tk);

  LitPoolDelete(&lpool);
  LexerDelete  (&l);
}

TEST(Lexer,Char) {
  const char* str = "'x' 'v' '\\t' '\\r' '\\v' '\\b' '\\n'";
  LitPool lpool;
  Lexer   l;

  Result res[] = {
    { .v.cval = 'x' , .tk = TK_LIT_CHAR },
    { .v.cval = 'v' , .tk = TK_LIT_CHAR },
    { .v.cval = '\t', .tk = TK_LIT_CHAR },
    { .v.cval = '\r', .tk = TK_LIT_CHAR },
    { .v.cval = '\v', .tk = TK_LIT_CHAR },
    { .v.cval = '\b', .tk = TK_LIT_CHAR },
    { .v.cval = '\n', .tk = TK_LIT_CHAR },
    { .tk = TK_EOF }
  };

  LitPoolInit(&lpool,mpool);
  LexerInit  (&l,&lpool,str);

  for( size_t i = 0 ; i < array_size(res) ; ++i ) {
    ASSERT_EQ(res[i].tk,LexerNext(&l)->tk);
    if(res[i].tk == TK_LIT_CHAR) {
      const Lit* lit = LitPoolIndex(&lpool,l.lexeme.lit);
      ASSERT_EQ(ELT_CHAR,lit->type);
      ASSERT_EQ(res[i].v.cval,lit->d.cval);
    }
  }

  LitPoolDelete(&lpool);
  LexerDelete  (&l);
}

TEST(Lexer,EmptyChar) {
  const char* str = " '' ";
  LitPool lpool;
  Lexer   l;

  LitPoolInit(&lpool,mpool);
  LexerInit  (&l,&lpool,str);

  ASSERT_EQ(TK_ERROR,LexerNext(&l)->tk);

  LitPoolDelete(&lpool);
  LexerDelete  (&l);
}

TEST(Lexer,Str) {
  const char* str = " \"\" \"abcd\" \"\\ba\" \"\\nb\" \"\\\\\" \"\\\"\" ";
  LitPool lpool;
  Lexer   l;

  Result res[] = {
    { .v.sval = ""     , .tk = TK_LIT_STR },
    { .v.sval = "abcd" , .tk = TK_LIT_STR },
    { .v.sval = "\ba"  , .tk = TK_LIT_STR },
    { .v.sval = "\nb"  , .tk = TK_LIT_STR },
    { .v.sval = "\\"   , .tk = TK_LIT_STR },
    { .v.sval = "\""   , .tk = TK_LIT_STR },
    { .tk = TK_EOF }
  };

  LitPoolInit(&lpool,mpool);
  LexerInit  (&l,&lpool,str);

  for( size_t i = 0 ; i < array_size(res) ; ++i ) {
    ASSERT_EQ(res[i].tk,LexerNext(&l)->tk);
    if(res[i].tk == TK_LIT_STR) {
      const Lit* lit = LitPoolIndex(&lpool,l.lexeme.lit);
      ASSERT_EQ(ELT_STR,lit->type);
      ASSERT_STREQ(res[i].v.sval,lit->d.str);
    }
  }

  LitPoolDelete(&lpool);
  LexerDelete  (&l);
}

TEST(Lexer,Keyword) {
  const char* str = "int char double string void bool true false if elif else for return struct";
  LitPool lpool;
  Lexer   l;
  Token tk[] = {
    TK_INT,
    TK_CHAR,
    TK_DBL,
    TK_STR,
    TK_VOID,
    TK_BOOL,
    TK_LIT_TRUE,
    TK_LIT_FALSE,
    TK_IF,
    TK_ELIF,
    TK_ELSE,
    TK_FOR,
    TK_RETURN,
    TK_STRUCT,
    TK_EOF
  };

  LitPoolInit(&lpool,mpool);
  LexerInit  (&l,&lpool,str);

  for( size_t i = 0 ; i < array_size(tk) ; ++i ) {
    LexerNext(&l);
    ASSERT_EQ(tk[i],l.lexeme.tk);
  }

  LitPoolDelete(&lpool);
  LexerDelete  (&l);
}

TEST(Lexer,Id) {
  const char* str = "id _a _true false_ if_ elif_ else_ for2 return3";
  LitPool lpool;
  Lexer   l;

  Result res[] = {
    { .v.sval = "id" , .tk = TK_ID },
    { .v.sval = "_a" , .tk = TK_ID },
    { .v.sval = "_true" , .tk = TK_ID },
    { .v.sval = "false_", .tk = TK_ID },
    { .v.sval = "if_"   , .tk = TK_ID },
    { .v.sval = "elif_" , .tk = TK_ID },
    { .v.sval = "else_" , .tk = TK_ID },
    { .v.sval = "for2"  , .tk = TK_ID },
    { .v.sval = "return3" , .tk = TK_ID },
    { .tk = TK_EOF }
  };

  LitPoolInit(&lpool,mpool);
  LexerInit  (&l,&lpool,str);

  for( size_t i = 0 ; i < array_size(res) ; ++i ) {
    const Lit* lit;
    LexerNext(&l);
    ASSERT_EQ(res[i].tk,l.lexeme.tk);
    if(res[i].tk != TK_EOF) {
      lit = LitPoolIndex(&lpool,l.lexeme.lit);
      ASSERT_EQ(ELT_ID,lit->type);
      ASSERT_STREQ(res[i].v.sval,lit->d.str);
    }
  }
  LitPoolDelete(&lpool);
  LexerDelete  (&l);
}

TEST(Lexer,Comment) {
  const char* str = "#line comment\n"
                    "id # trailing comment\n"
                    "# end of file comment";
  LitPool lpool;
  Lexer   l;

  LitPoolInit(&lpool,mpool);
  LexerInit  (&l,&lpool,str);

  LexerNext(&l);
  ASSERT_EQ(TK_ID,l.lexeme.tk);
  LexerNext(&l);
  ASSERT_EQ(TK_EOF,l.lexeme.tk);

  LitPoolDelete(&lpool);
  LexerDelete  (&l);
}

int main( int argc , char* argv[] ) {
  int ret;
  MPool p;
  MPoolInit(&p,8,32);
  mpool = &p;
  ret = RunAllTests(argc,argv);
  MPoolDelete(&p);
  return ret;
}
