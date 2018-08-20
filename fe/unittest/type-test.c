#include "../src/tcmm.h"

#include <cunitpp.h>

TEST(Type,Primitive) {
  TypeSys tsys;
  MPool   mpool;
  LitPool lpool;

  MPoolInit(&mpool,8,32);
  LitPoolInit(&lpool,&mpool);
  TypeSysInit(&tsys,&lpool,&mpool);

  ASSERT_EQ(EPT_INT,TypeSysGetInt(&tsys)->base.tag);
  ASSERT_EQ(sizeof(int),TypeSysGetInt(&tsys)->base.size);

  ASSERT_EQ(EPT_DBL,TypeSysGetDbl(&tsys)->base.tag);
  ASSERT_EQ(sizeof(double),TypeSysGetDbl(&tsys)->base.size);

  ASSERT_EQ(EPT_CHAR,TypeSysGetChar(&tsys)->base.tag);
  ASSERT_EQ(1,TypeSysGetChar(&tsys)->base.size);

  ASSERT_EQ(EPT_BOOL,TypeSysGetBool(&tsys)->base.tag);
  ASSERT_EQ(1,TypeSysGetBool(&tsys)->base.size);

  ASSERT_EQ(EPT_VOID,TypeSysGetVoid(&tsys)->base.tag);
  ASSERT_EQ(1,TypeSysGetVoid(&tsys)->base.size);

  LitPoolDelete(&lpool);
  TypeSysDelete(&tsys);
  MPoolDelete(&mpool);
}

TEST(Type,Array) {
  MPool mpool;
  TypeSys tsys;
  LitPool lpool;

  MPoolInit(&mpool,8,32);
  LitPoolInit(&lpool,&mpool);
  TypeSysInit(&tsys,&lpool,&mpool);

  {
    /**
     * Create int array type
     */
    ArrType* at = TypeSysSetArr(&tsys, (const Type*)(TypeSysGetInt(&tsys)),10);
    const ArrType* r  = NULL;
    ASSERT_EQ(10,at->len);
    ASSERT_EQ(at->type,(const Type*)(TypeSysGetInt(&tsys)));

    r = TypeSysGetArr(&tsys,(const Type*)(TypeSysGetInt(&tsys)),10);
    ASSERT_EQ(at,r);
  }

  LitPoolDelete(&lpool);
  TypeSysDelete(&tsys);
  MPoolDelete(&mpool);
}

StructType* Struct0( TypeSys* sys , const char* name ) {
  LitIdx name_idx = LitPoolGetStr( sys->lpool, name );
  return TypeSysSetStruct(sys,name_idx);
}

StructType* Struct1( TypeSys* sys , const char* name ,
                                    const Type* t1   , const char* n1 ) {
  StructType* stype = Struct0(sys,name);
  TypeSysAddStructField(sys,stype,LitPoolGetStr(sys->lpool,n1),t1);
  return stype;
}

StructType* Struct2( TypeSys* sys , const char* name ,
                                    const Type* t1   , const char* n1 ,
                                    const Type* t2   , const char* n2 ) {
  StructType* stype = Struct1(sys,name,t1,n1);
  TypeSysAddStructField(sys,stype,LitPoolGetStr(sys->lpool,n2),t2);
  return stype;
}

TEST(Type,Struct) {
  TypeSys tsys;
  MPool   mpool;
  LitPool lpool;

  MPoolInit  (&mpool,1024,4096);
  LitPoolInit(&lpool,&mpool);
  TypeSysInit(&tsys,&lpool,&mpool);

  {
    StructType* stype = Struct0(&tsys,"astruct0");
    ASSERT_EQ(0,stype->fsize);
    ASSERT_STREQ("astruct0",LitPoolStr(&lpool,stype->name));
    ASSERT_EQ(ET_STRUCT,stype->base.tag);
  }

  {
    const Type* t1 = (const Type*)TypeSysGetChar(&tsys);
    const Type* t2 = (const Type*)TypeSysGetInt (&tsys);

    StructType* stype = Struct2(&tsys,"astruct2",t1,"achar",t2,"aint");
    ASSERT_EQ(2,stype->fsize);
    ASSERT_STREQ("astruct2",LitPoolStr(&lpool,stype->name));
    ASSERT_EQ(ET_STRUCT,stype->base.tag);
  }

  {
    const Type* t1 = (const Type*)TypeSysSetArr(&tsys,(const Type*)(TypeSysGetInt(&tsys)),10);
    const Type* t2 = (const Type*)TypeSysGetDbl(&tsys);
    StructType* stype = Struct2(&tsys,"astruct_arr2",t1,"arr",t2,"dbl");
    ASSERT_EQ(2,stype->fsize);
    ASSERT_STREQ("astruct_arr2",LitPoolStr(&lpool,stype->name));
    ASSERT_EQ(ET_STRUCT,stype->base.tag);
  }

  TypeSysToJSON(&tsys,stderr);
  fprintf(stderr,"\n");

  LitPoolDelete(&lpool);
  TypeSysDelete(&tsys);
}

int main( int argc , char* argv[] ) {
  return RunAllTests(argc,argv);
}
