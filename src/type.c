#include "tcmm.h"

#define LINK_TYPE(SYS,T)           \
  do {                             \
    (T)->base.next = (SYS)->next;  \
    (SYS)->next    = (T);          \
  } while(0)


PAPI void TypeSysInit( TypeSys* sys , LitPool* lpool ) {
  sys->lpool = lpool;
  sys->types = NULL;
  MPoolInit(&(sys->mpool),128,2048);

  // initialize preloaded type
  sys->t_int.base.tag  = EPT_INT;
  sys->t_int.base.size = sizeof(int32_t);

  sys->t_dbl.base.tag  = EPT_DBL;
  sys->t_dbl.base.size = sizeof(double);

  sys->t_bool.base.tag  = EPT_BOOL;
  sys->t_bool.base.size = 1;

  sys->t_void.base.tag  = EPT_VOID;
  sys->t_void.base.size = POINTER_SIZE;

  sys->t_str.base.tag  = ET_STR;
  sys->t_str.base.size = 0;
}

PAPI void TypeSysDelete( TypeSys* sys ) {
  MPoolDelete(&(sys->pool));
}

PAPI const StructType* TypeSysGetStruct( TypeSys* sys , LitIdx idx ) {
  for( const Type* t = sys->types; t ; t = t->next ) {
    if(t->tag == ET_STRUCT) {
      const StructType* st = t;
      if(st->name == idx)
        return st;
    }
  }
  return NULL;
}

PAPI const ArrType* TypeSysGetArr( TypeSys* sys , const Type* t , size_t length ) {
  for( const Type* t = sys->types; t ; t = t->next ) {
    if(t->tag == ET_ARR) {
      const ArrType* at = t;
      if(at->type == t && at->len == length)
        return at;
    }
  }
  return NULL;
}

PAPI const FuncType* TypeSysGetFunc( TypeSys* sys , LitIdx idx ) {
  for( const Type* t = sys->types ; t ; t = t->next ) {
    if(t->tag == ET_FUNC) {
      const FuncType* ft = t;
      if(ft->name == idx)
        return ft;
    }
  }
  return NULL;
}

PAPI
ArrType* TypeSysSetArr( TypeSys* sys , const Type* t , size_t length ) {
  assert( TypeSysGetArr(sys,t,length) == NULL );
  {
    ArrType* at = MPoolGrab(&(sys->pool),sizeof(*at));
    size_t  asz;

    at->base.tag= ET_ARR;
    LINK_TYPE(sys,at);

    at->len       = length;
    asz           = t->base.size * length;
    at->base.size = ALIGN(asz,CONFIG_ARRAY_ALIGNMENT);

    return at;
  }
}
