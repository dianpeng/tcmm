#include "../src/tcmm.h"

#include <stdio.h>
#include <cunitpp.h>

extern Node* ParseExpression( Parser* );

TEST(Parser,Simple) {
  MPool   mpool;
  LitPool lpool;
  TypeSys  tsys;
  Parser      p;
  Node*     exp;
  const char* src = "true || false + 1";

  MPoolInit(&mpool,8,32);
  LitPoolInit(&lpool,&mpool);
  TypeSysInit(&tsys,&lpool,&mpool);
  ParserInit (&p,&lpool,&tsys,src);

  exp = ParseExpression(&p);
  ASSERT_TRUE(exp);
  NodeToJSON(&p,stderr,exp);
  fprintf(stderr,"\n");

  ParserDelete(&p);
  TypeSysDelete(&tsys);
  LitPoolDelete(&lpool);
}

int main( int argc , char* argv[] ) {
  return RunAllTests(argc,argv);
}
