#include "tcmm.h"

PAPI void SymTableInit( SymTable* t , MPool* pool ) {
  t->sym = NULL;
  t->sz  = 0;
  t->cap = 0;
  t->pool= pool;
}

PAPI void SymTableDelete( SymTable* t ) {
  t->sym = NULL;
  t->sz  = 0;
  t->cap = 0;
  t->pool= NULL;
}
