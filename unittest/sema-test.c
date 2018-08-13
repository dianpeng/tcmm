#include "../src/tcmm.h"
#include <stdio.h>
#include <cunitpp.h>

TEST(Sema,Simple) {
  MPool   mpool;
  LitPool lpool;
  NodeFile*   n;
  const char* src = "struct xx { int a; int b ; }; "
                    "struct xx g = { .a = 10 };"
                    "struct xx uu= { .a = 20 , .b = 30 };";

  MPoolInit(&mpool,8,32);
  LitPoolInit(&lpool,&mpool);
  n = Parse(&lpool,&mpool,src);
  ASSERT_TRUE(n);

  // do the semantic check
	{
    const char* err;
    TypeSys tsys;
    TypeSysInit(&tsys,&lpool,&mpool);
    ASSERT_TRUE( SemaCheck(&lpool,&tsys,&mpool,src,n,&err) == 0 );
    TypeSysDelete(&tsys);
	}

  NodeToJSON(&lpool,&mpool,stderr,(Node*)n);
  fprintf(stderr,"\n");

  LitPoolDelete(&lpool);
  MPoolDelete(&mpool);
}

int main( int argc , char* argv[] ) {
  return RunAllTests(argc,argv);
}
