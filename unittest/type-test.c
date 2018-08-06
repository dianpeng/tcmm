#include "../src/tcmm.h"

#include <cunitpp.h>

TEST(Type,Primitive) {
  TypeSys tsys;
  LitPool lpool;

  LitPoolInit(&lpool);
  TypeSysInit(&tsys,&lpool);

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
}

TEST(Type,Array) {
  TypeSys tsys;
  LitPool lpool;

  LitPoolInit(&lpool);
  TypeSysInit(&tsys,&lpool);

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
}

#if 0

StructType* Struct0( TypeSys* sys , const char* name ) {
  LitIdx name_idx = LitPoolGetStr( sys->lpool, name );
  return TypeSysSetStruct(sys,name_idx);
}

StructType* Struct1( TypeSys* sys , const char* name ,
                                    const Type* t1   , const char* n1 ) {
  StructType* stype = Struct0(sys,name);
  TypeSysAddStructField(sys,stype,LitPoolGetStr(sys->lpool,n1),t1);
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
  LitPool lpool;

  LitPoolInit(&lpool);
  TypeSysInit(&tsys,&lpool);

  {
    StructType* stype = Struct0(&tsys,"astruct0");
    ASSERT_EQ(0,stype->fsize);
    {
      const Lit* lit = LitPoolIndex(&lpool,stype->name);
      ASSERT_EQ(ELT_STR,lit->type);
      ASSERT_STREQ("astruct0",lit->d.str);
    }
    ASSERT_EQ(ET_STRUCT,stype->base.tag);
  }


  LitPoolDelete(&lpool);
  TypeSysDelete(&tsys);
}

#endif

int main( int argc , char* argv[] ) {
  return RunAllTests(argc,argv);
}
