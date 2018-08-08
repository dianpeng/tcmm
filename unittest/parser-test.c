#include "../src/tcmm.h"

#include <stdio.h>
#include <cunitpp.h>

extern Node* ParseExpression( LitPool* , MPool* , const char* );

TEST(Parser,Simple) {
  MPool   mpool;
  LitPool lpool;
  Node*     exp;
  const char* src = "true || false + 1";

  MPoolInit(&mpool,8,32);
  LitPoolInit(&lpool,&mpool);

  exp = ParseExpression(&lpool,&mpool,src);
  ASSERT_TRUE(exp);
  NodeToJSON(&lpool,&mpool,stderr,exp);
  fprintf(stderr,"\n");

  LitPoolDelete(&lpool);
  MPoolDelete(&mpool);
}

TEST(Parser,Stmt) {
  MPool   mpool;
  LitPool lpool;
  NodeFile*   n;
  const char* src = "int foo() {\n"
                    "for( int a = 10 ; a < 20 ; a += 2) { break; }"
                    "if(a) return 10 ; else for( ;; ) continue;   "
                    "return 100;}";

  MPoolInit(&mpool,8,32);
  LitPoolInit(&lpool,&mpool);
  n = Parse(&lpool,&mpool,src);
  ASSERT_TRUE(n);
  NodeToJSON(&lpool,&mpool,stderr,(Node*)n);
  fprintf(stderr,"\n");
  LitPoolDelete(&lpool);
  MPoolDelete(&mpool);
}

int main( int argc , char* argv[] ) {
  return RunAllTests(argc,argv);
}
