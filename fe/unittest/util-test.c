#include "../src/tcmm.h"
#include <stdio.h>
#include <stdlib.h>
#include <cunitpp.h>

TEST(MPool,Refill) {
  MPool pool;
  MPoolInit(&pool,4,8);
  ASSERT_EQ(0,pool.ftm);
  {
    int* b , *c , *d;
    int* a = MPoolGrab(&pool,sizeof(int)); // should trigger a default segment initialization
    *a = 1;
    ASSERT_EQ(1,pool.ftm);
    b = MPoolGrab(&pool,sizeof(int)); *b = 2;
    c = MPoolGrab(&pool,sizeof(int)); *c = 3;
    ASSERT_EQ(2,pool.ftm);

    d = MPoolGrab(&pool,sizeof(int)); *d = 4;
    ASSERT_EQ(3,pool.ftm);

    // now test whether the refill only fills up at most 8 bytes
    {
      int* f;
      int* e = MPoolGrab(&pool,sizeof(int)); *e = 5;
      ASSERT_EQ(3,pool.ftm);
      f = MPoolGrab(&pool,sizeof(int)); *f = 6;
      ASSERT_EQ(4,pool.ftm);

      ASSERT_EQ(*e,5);
      ASSERT_EQ(*f,6);
    }
    ASSERT_EQ(*a,1);
    ASSERT_EQ(*b,2);
    ASSERT_EQ(*c,3);
    ASSERT_EQ(*d,4);
  }
  MPoolDelete(&pool);
}

TEST(MPool,Stress) {
  MPool pool;
  MPoolInit(&pool,4,16);
  ASSERT_EQ(0,pool.ftm);
  {
    const size_t len = 1024*1024;
    int** arr = malloc(sizeof(int*)*len);
    for( size_t i = 0 ; i < len ; ++i ) {
      arr[i] = MPoolGrab(&pool,sizeof(int*)); *arr[i] = i;
    }

    for( size_t i = 0 ; i < len ; ++i ) {
      ASSERT_EQ(*arr[i],i);
    }
    free(arr);
  }
  MPoolDelete(&pool);
}

TEST(LitPool,Insert) {
  MPool   mpool;
  LitPool pool;
  MPoolInit(&mpool,8,32);
  LitPoolInit(&pool,&mpool);

  ASSERT_EQ(2,pool.sz);

  for( size_t i = 0 ; i < CONFIG_LIT_POOL_SIZE * 2 ; ++i ) {
    LitIdx idx = LitPoolGetInt(&pool,(int)i);
    const Lit* l = LitPoolIndex(&pool,idx);
    ASSERT_EQ(ELT_INT,l->type);
    ASSERT_EQ((int)i,l->d.ival);
  }

  ASSERT_EQ(CONFIG_LIT_POOL_SIZE*2+2,pool.sz);
  ASSERT_EQ(ELT_FALSE,LitPoolIndex(&pool,LitPoolGetFalse(&pool))->type);
  ASSERT_EQ(ELT_TRUE ,LitPoolIndex(&pool,LitPoolGetTrue (&pool))->type);

  LitPoolDelete(&pool);
  MPoolDelete(&mpool);
}

static void DoStrOrIdTest ( int t ) {
  char buf[1024];
  MPool   mpool;
  LitPool pool;
  MPoolInit(&mpool,8,32);
  LitPoolInit(&pool,&mpool);
  ASSERT_EQ(2,pool.sz);

  for( size_t i = 0 ; i < CONFIG_LIT_POOL_SIZE * 10 ; ++i ) {
    LitIdx idx;
    const Lit* l;
    size_t sz;

    sz = sprintf(buf,"%d",(int)i);
    if(t == ELT_STR) {
      idx = LitPoolGetStr(&pool,buf);
    } else {
      idx = LitPoolGetId (&pool,buf , buf + sz);
    }
    l   = LitPoolIndex (&pool,idx);
    ASSERT_EQ(l->type,t);
    ASSERT_STREQ(buf,l->d.str);
  }

  ASSERT_EQ(CONFIG_LIT_POOL_SIZE*10+2,pool.sz);

  LitPoolDelete(&pool);
  MPoolDelete(&mpool);
}

TEST(LitPool,Str) {
  DoStrOrIdTest(ELT_STR);
  DoStrOrIdTest(ELT_ID );
}

int main( int argc , char* argv[] ) {
  return RunAllTests(argc,argv);
}
