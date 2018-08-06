#include "tcmm.h"
#include <stdlib.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>

PAPI
void MPoolInit( MPool* pool , size_t cap , size_t max ) {
  pool->first = NULL;
  pool->last  = NULL;
  pool->cur   = NULL;
  pool->end   = NULL;
  pool->cap   = cap ;
  pool->max   = max ;
  pool->ftm   = 0;
}

static
void MPoolRefill( MPool* pool , size_t hint ) {
  MPoolSeg* seg;
  size_t    cap = pool->cap;

  // adjust the new capacity
  if(cap > pool->max) {
    cap = pool->max;
  }
  if(cap < hint) {
    cap = hint;
  }

  seg= malloc(sizeof(*seg) + cap);
  seg->next = NULL;
  seg->end  = ((char*)seg + sizeof(*seg) + cap);

  if(pool->cur == NULL) {
    assert(!pool->first && !pool->last);
    pool->first = seg;
    pool->last  = seg;
  } else {
    pool->last->next = seg;
    pool->last = seg;
  }

  pool->cur   = ((char*)seg + sizeof(*seg));
  pool->end   = seg->end;
  pool->cap   = cap * 2;
  ++pool->ftm;
}

static
void MPoolDeleteSeg( MPool* pool ) {
  MPoolSeg* s = pool->first;
  while(s) {
    MPoolSeg* n = s->next;
    free(s);
    s = n;
  }
  pool->first = pool->last = NULL;
}

PAPI
void MPoolReset( MPool* pool , size_t cap , size_t max ) {
  MPoolDeleteSeg(pool);
  MPoolInit(pool,cap,max);
}

PAPI
void MPoolDelete( MPool* pool ) {
  MPoolDeleteSeg(pool);
}

PAPI
void* MPoolGrab( MPool* pool , size_t sz ) {
  void* r;
  char* e = (char*)(pool->cur) + sz;
  if(e > (char*)(pool->end)) {
    MPoolRefill(pool,sz);
    e = (char*)(pool->cur) + sz;
  }
  r = pool->cur;
  pool->cur = e;
  return r;
}

PAPI
void* MPoolRealloc( MPool* pool , void* obuf , size_t old_sz , size_t new_sz ) {
  void* nbuf = MPoolGrab(pool,new_sz);
  if(old_sz) memcpy(nbuf,obuf,old_sz);
  return nbuf;
}

PAPI
const char* MPoolStrDup( MPool* pool , const char* str ) {
  size_t l = strlen(str);
  char* s = MPoolGrab(pool,l+1);
  memcpy(s,str,l);
  s[l] = 0;
  return s;
}

PAPI
const char* MPoolStrSub( MPool* pool , const char* start , const char* end ) {
  char* buf = MPoolGrab(pool,(end-start)+1);
  memcpy(buf,start,(end-start));
  buf[end-start] = 0;
  return buf;
}

PAPI
void LitPoolInit( LitPool* pool ) {
  pool->lits = malloc(sizeof(Lit) * CONFIG_LIT_POOL_SIZE);
  pool->cap  = CONFIG_LIT_POOL_SIZE;

  pool->sz   = 2;
  pool->lits[0].type = ELT_FALSE;
  pool->lits[1].type = ELT_TRUE;

  MPoolInit(&(pool->mpool),128,1024*16);
}

PAPI
void LitPoolDelete( LitPool* pool ) {
  free(pool->lits);
  pool->lits = NULL;
  pool->sz   = 0;
  pool->cap  = 0;

  MPoolDelete(&(pool->mpool));
}

static
Lit* LitPoolInsert( LitPool* pool ) {
  if(pool->sz == pool->cap) {
    pool->lits = realloc(pool->lits,sizeof(Lit) * pool->cap * 2);
    pool->cap *= 2;
  }
  return pool->lits + (pool->sz++);
}

PAPI
LitIdx LitPoolGetDouble( LitPool* pool , double d ) {
  for( size_t i = 0 ; i < pool->sz ; ++i ) {
    if(pool->lits[i].type == ELT_DBL) {
      if(d == pool->lits[i].d.rval)
        return i;
    }
  }

  {
    Lit* l  = LitPoolInsert(pool);
    l->type = ELT_DBL;
    l->d.rval = d;
    return pool->sz - 1;
  }
}

PAPI
LitIdx LitPoolGetInt( LitPool* pool , int32_t ival ) {
  for( size_t i = 0 ; i < pool->sz ; ++i ) {
    if(pool->lits[i].type == ELT_INT) {
      if(ival == pool->lits[i].d.ival)
        return i;
    }
  }

  {
    Lit* l  = LitPoolInsert(pool);
    l->type = ELT_INT;
    l->d.ival = ival;
    return pool->sz - 1;
  }
}

PAPI
LitIdx LitPoolGetChar( LitPool* pool , char c ) {
  for( size_t i = 0 ; i < pool->sz ; ++i ) {
    if(pool->lits[i].type == ELT_CHAR) {
      if(c == pool->lits[i].d.cval) {
        return i;
      }
    }
  }

  {
    Lit* l  = LitPoolInsert(pool);
    l->type = ELT_CHAR;
    l->d.cval = c;
    return pool->sz - 1;
  }
}

PAPI
LitIdx LitPoolGetStr( LitPool* pool , const char* str ) {
  for( size_t i = 0 ; i < pool->sz ; ++i ) {
    if(pool->lits[i].type == ELT_STR) {
      if(strcmp(pool->lits[i].d.str,str) == 0) {
        return i;
      }
    }
  }

  {
    Lit* l  = LitPoolInsert(pool);
    l->type = ELT_STR;
    l->d.str= MPoolStrDup(&(pool->mpool),str);
    return pool->sz - 1;
  }
}

PAPI
LitIdx LitPoolGetId( LitPool* pool , const char* start , const char* end ) {
  for( size_t i = 0 ; i < pool->sz ; ++i ) {
    if(pool->lits[i].type == ELT_ID) {
      if((strncmp(pool->lits[i].d.str,start,(end-start)) == 0) &&
         (pool->lits[i].d.str[end-start] == 0)) {
        return i;
      }
    }
  }

  {
    Lit* l  = LitPoolInsert(pool);
    l->type = ELT_ID;
    l->d.str= MPoolStrSub(&(pool->mpool),start,end);
    return pool->sz - 1;
  }
}

PAPI
const Lit* LitPoolIndex( LitPool* pool , LitIdx idx ) {
  assert(idx < pool->sz);
  return pool->lits + idx;
}

PAPI
const char* LitPoolStr( LitPool* pool , LitIdx idx ) {
  const Lit* l = LitPoolIndex(pool,idx);
  assert(l->type == ELT_STR);
  return l->d.str;
}

PAPI
const char* LitPoolId( LitPool* pool , LitIdx idx ) {
  const Lit* l = LitPoolIndex(pool,idx);
  assert(l->type == ELT_ID);
  return l->d.str;
}

PAPI
double LitPoolDbl( LitPool* pool , LitIdx idx ) {
  const Lit* l = LitPoolIndex(pool,idx);
  assert(l->type == ELT_DBL);
  return l->d.rval;
}

PAPI
int32_t LitPoolInt( LitPool* pool , LitIdx idx ) {
  const Lit* l = LitPoolIndex(pool,idx);
  assert(l->type == ELT_INT);
  return l->d.ival;
}

PAPI
char LitPoolChar( LitPool* pool , LitIdx idx ) {
  const Lit* l = LitPoolIndex(pool,idx);
  assert(l->type == ELT_CHAR);
  return l->d.cval;
}

PAPI
int LitPoolBool( LitPool* pool , LitIdx idx ) {
  const Lit* l = LitPoolIndex(pool,idx);
  assert(l->type == ELT_TRUE || l->type == ELT_FALSE);
  return l->type == ELT_TRUE;
}

PAPI
int StrToI32( const char* src , int base , int32_t* output ) {
  char* pend;
  long r;

  errno = 0;
  r = strtol(src,&pend,base);
  if(errno) {
    return -1;
  }

  *output = (int)(r);
  return (pend - src);
}

PAPI
int StrToDbl( const char* src , double* output ) {
  char* pend;
  double r;

  errno = 0;
  r = strtod(src,&pend);
  if(errno) {
    return -1;
  }

  *output = r;
  return (pend-src);
}


#include <stdio.h>

PAPI
const char* ReportError( const char* context , const char* source , size_t pos ,
                                                                    size_t line ,
                                                                    size_t nchar ,
                                                                    const char* msg ) {
  fprintf(stderr,"Failed[%s]:%s\n",context,msg);
  return strdup(msg);
}
