#include "tcmm.h"
#include <assert.h>

#define LINK_TYPE(SYS,T)            \
  do {                              \
    (T)->base.next = (SYS)->types;  \
    (SYS)->types   = (Type*)(T);    \
  } while(0)


PAPI void TypeSysInit( TypeSys* sys , LitPool* lpool ) {
  sys->lpool = lpool;
  sys->types = NULL;
  MPoolInit(&(sys->pool),128,2048);

  // initialize preloaded type
  sys->t_int.base.tag   = EPT_INT;
  sys->t_int.base.size  = sizeof(int32_t);
  sys->t_int.base.align = sizeof(int32_t);

  sys->t_dbl.base.tag   = EPT_DBL;
  sys->t_dbl.base.size  = sizeof(double);
  sys->t_dbl.base.align = sizeof(double);

  sys->t_bool.base.tag  = EPT_BOOL;
  sys->t_bool.base.size = 1;
  sys->t_bool.base.align= 1;

  sys->t_char.base.tag  = EPT_CHAR;
  sys->t_char.base.size = sizeof(char);
  sys->t_char.base.align= sizeof(char);

  sys->t_void.base.tag  = EPT_VOID;
  sys->t_void.base.size = 1;
  sys->t_void.base.align= 1;

  sys->t_str.base.tag   = ET_STR;
  sys->t_str.base.size  = PTR_SIZE;
  sys->t_str.base.align = PTR_SIZE;
}

PAPI void TypeSysDelete( TypeSys* sys ) {
  MPoolDelete(&(sys->pool));
}

PAPI const StructType* TypeSysGetStruct( TypeSys* sys , LitIdx idx ) {
  for( const Type* t = sys->types; t ; t = t->next ) {
    if(t->tag == ET_STRUCT) {
      const StructType* st = (const StructType*)t;
      if(st->name == idx)
        return st;
    }
  }
  return NULL;
}

PAPI const ArrType* TypeSysGetArr( TypeSys* sys , const  Type* type,
                                                  size_t length ) {
  for( const Type* t = sys->types; t ; t = t->next ) {
    if(t->tag == ET_ARR) {
      const ArrType* at = (const ArrType*)t;
      if(at->type == type && at->len == length)
        return at;
    }
  }
  return NULL;
}

PAPI const FuncType* TypeSysGetFunc( TypeSys* sys , LitIdx idx ) {
  for( const Type* t = sys->types ; t ; t = t->next ) {
    if(t->tag == ET_FUNC) {
      const FuncType* ft = (const FuncType*)t;
      if(ft->name == idx)
        return ft;
    }
  }
  return NULL;
}

PAPI const FieldType* TypeSysGetStructField( TypeSys* sys , const StructType* st , LitIdx idx ) {
  for( size_t i = 0 ; i < st->fsize ; ++i ) {
    const FieldType* ft = st->fstart + i;
    if(ft->name == idx)
      return ft;
  }
  return NULL;
}

PAPI
ArrType* TypeSysSetArr( TypeSys* sys , const Type* t , size_t length ) {
  assert( TypeSysGetArr(sys,t,length) == NULL );
  assert( t->tag != EPT_VOID );

  {
    ArrType* at = MPoolGrab(&(sys->pool),sizeof(*at));

    at->base.tag= ET_ARR;
    LINK_TYPE(sys,at);

    at->type      = t;
    at->len       = length;
    at->base.size = t->size * length;
    at->base.align= t->align;
    return at;
  }
}

PAPI
StructType* TypeSysSetStruct( TypeSys* sys , LitIdx idx ) {
  assert( TypeSysGetStruct(sys,idx) == NULL );
  {
    StructType* st = MPoolGrab(&(sys->pool),sizeof(*st));
    st->base.tag   = ET_STRUCT;
    st->base.size  = 1;
    st->base.align = 1;

    st->fstart   = NULL;
    st->fsize    = 0;
    st->fcap     = 0;
    st->name     = idx;
    return st;
  }
}

PAPI
const FieldType* TypeSysAddStructField( TypeSys* sys , StructType* st , LitIdx idx , const Type* t ) {
  FieldType* ft;
  if(st->fcap == st->fsize) {
    size_t ncap= st->fcap ? st->fcap * 2 : 8;
    st->fstart = MPoolRealloc(&(sys->pool),st->fstart,st->fsize*sizeof(FieldType),ncap);
    st->fcap   = ncap;
  }

  ft    = st->fstart + st->fsize;
  ft->p = st;

  // calculate the padding and offset of each member field
  if(t->align > st->base.align) {
    st->base.align = t->align;
  }

  if(st->fsize > 0) {
    FieldType* pft = st->fstart + (st->fsize-1);  // previous field's type object
    size_t ppos    = pft->offset+ pft->t->size;   // where the previous field end
    size_t npos    = ALIGN(ppos,t->align);        // where the new field starts after padding/alignment
    ft->offset     = npos;
  }

  ft->t      = t;
  ft->name   = idx;

  ++st->fsize;
  return ft;
}

PAPI
FuncType* TypeSysSetFunc( TypeSys* sys , LitIdx idx ) {
  assert( TypeSysGetFunc(sys,idx) == NULL );
  {
    FuncType* ft = MPoolGrab(&(sys->pool),sizeof(*ft));

    ft->base.tag   = ET_FUNC;
    ft->base.size  = PTR_SIZE;
    ft->base.align = PTR_SIZE;

    LINK_TYPE(sys,ft);

    ft->name     = idx;
    ft->ret      = NULL;
    ft->arg      = NULL;
    ft->arg_size = 0;
    ft->arg_cap  = 0;

    return ft;
  }
}

PAPI
void TypeSysFuncSetRet( TypeSys* sys , FuncType* ft , const Type* t ) {
  (void)sys;
  ft->ret = t;
}

PAPI
const FuncTypeArg*
TypeSysFuncAddArg( TypeSys* sys , FuncType* ft , const Type* t , LitIdx aname ) {
  if(ft->arg_size == ft->arg_cap) {
    size_t ncap = ft->arg_cap == 0 ? 8 : ft->arg_cap * 2;
    ft->arg     = MPoolRealloc(&(sys->pool),ft->arg,sizeof(FuncType   )*ft->arg_cap,
                                                    sizeof(FuncTypeArg)*ncap);
    ft->arg_cap = ncap;
  }
  ft->arg[ft->arg_size].type = t;
  ft->arg[ft->arg_size].name = aname;
  ++ft->arg_size;

  return ft->arg + (ft->arg_size-1);
}

/**
 * This function implements the implicit cast strategy
 *
 * char --> int
 * int  --> double
 * bool --> int
 * bool --> char
 *
 * ANY  --> bool
 */
PAPI
int TypeSysCanCast( TypeSys* sys , const Type* from , const Type* to ) {
  (void)sys;
  switch(from->tag) {
    case EPT_CHAR:
      return to->tag == EPT_INT;
    case EPT_INT :
      return to->tag == EPT_DBL;
    case EPT_BOOL:
      return to->tag == EPT_INT || to->tag == EPT_CHAR;
    case ET_ARR:
      if(to->tag == ET_ARR) {
        const ArrType* lhs_at = (const ArrType*)from;
        const ArrType* rhs_at = (const ArrType*)to;
        if(lhs_at->len == rhs_at->len) {
          return TypeSysCanCast(sys,lhs_at->type,rhs_at->type);
        }
      }
      return 0;
    default:
      break;
  }

  // same type can be casted
  if(from->tag == to->tag)
    return 1;

  // any type can be casted to bool
  return to->tag == EPT_BOOL;
}

#undef LINK_TYPE // LINK_TYPE
