#include "tcmm.h"
#include <assert.h>

#define LINK_TYPE(SYS,T)            \
  do {                              \
    (T)->base.next = (SYS)->types;  \
    (SYS)->types   = (Type*)(T);    \
  } while(0)


PAPI void TypeSysInit( TypeSys* sys , LitPool* lpool , MPool* pool ) {
  sys->lpool = lpool;
  sys->types = NULL;
  sys->pool  = pool;

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
  sys->types = NULL;
  sys->pool  = NULL;
  sys->lpool = NULL;
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
    ArrType* at = MPoolGrab(sys->pool,sizeof(*at));

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
    StructType* st = MPoolGrab(sys->pool,sizeof(*st));
    st->base.tag   = ET_STRUCT;
    st->base.size  = 1;
    st->base.align = 1;

    st->fstart   = NULL;
    st->fsize    = 0;
    st->fcap     = 0;
    st->name     = idx;

    LINK_TYPE(sys,st);
    return st;
  }
}

PAPI
const FieldType* TypeSysAddStructField( TypeSys* sys , StructType* st , LitIdx idx , const Type* t ) {
  FieldType* ft;
  if(st->fcap == st->fsize) {
    size_t ncap= st->fcap ? st->fcap * 2 : 8;
    st->fstart = MPoolRealloc(sys->pool,st->fstart,st->fsize*sizeof(FieldType),ncap*sizeof(FieldType));
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
    FuncType* ft = MPoolGrab(sys->pool,sizeof(*ft));

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
    ft->arg     = MPoolRealloc(sys->pool,ft->arg,sizeof(FuncType   )*ft->arg_cap,
                                                 sizeof(FuncTypeArg)*ncap);
    ft->arg_cap = ncap;
  }
  ft->arg[ft->arg_size].type = t;
  ft->arg[ft->arg_size].name = aname;
  ++ft->arg_size;

  return ft->arg + (ft->arg_size-1);
}

PAPI
const char* ETypeGetStr( EType t ) {
  switch(t) {
    case EPT_INT: return "int";
    case EPT_DBL: return "double";
    case EPT_CHAR: return "char";
    case EPT_BOOL: return "bool";
    case EPT_VOID: return "void";
    case ET_STR : return "str";
    case ET_STRUCT: return "struct";
    case ET_ARR: return "array";
    case ET_FUNC: return "func";
    default: assert(0); return "";
  }
}

#undef LINK_TYPE // LINK_TYPE
