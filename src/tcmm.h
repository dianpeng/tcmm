#ifndef TCMM_H_
#define TCMM_H_
#include <stdlib.h>
#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "config.h"
#include "arch.h"

/* -------------------------------------------------------
 * Util
 * ------------------------------------------------------*/

/* memory pool API, implement a simple bump pointer style allocator */
typedef struct _MPoolSeg {
  struct _MPoolSeg* next;
  void*              end;
} MPoolSeg;

typedef struct _MPool {
  MPoolSeg* first;
  MPoolSeg* last;
  void*      cur;
  void*      end;
  size_t     cap;
  size_t     max;
  size_t     ftm;
} MPool;

PAPI void  MPoolInit   ( MPool* , size_t , size_t );
PAPI void  MPoolDelete ( MPool* );
PAPI void  MPoolReset  ( MPool* , size_t , size_t );
PAPI void* MPoolGrab   ( MPool* , size_t );
PAPI void* MPoolRealloc( MPool* , void* , size_t , size_t );

// helpers
PAPI const char* MPoolStrDup( MPool* , const char* );

typedef enum _ELitType {
  ELT_INT,
  ELT_DBL,
  ELT_STR,
  ELT_CHAR,
  ELT_TRUE,
  ELT_FALSE,
  ELT_ID,
} ELitType;

typedef struct _Lit {
  union {
    double  rval;
    int32_t ival;
    char    cval;
    const char* str;
  } d;
  ELitType type;
} Lit;

typedef struct _LitPool {
  Lit* lits;
  size_t sz;
  size_t cap;
  MPool mpool;
} LitPool;

typedef size_t LitIdx;

PAPI void   LitPoolInit     ( LitPool* );
PAPI void   LitPoolDelete   ( LitPool* );
PAPI LitIdx LitPoolGetDouble( LitPool* , double );
PAPI LitIdx LitPoolGetInt   ( LitPool* , int32_t);
PAPI LitIdx LitPoolGetChar  ( LitPool* , char   );
PAPI LitIdx LitPoolGetStr   ( LitPool* , const char* );
PAPI LitIdx LitPoolGetId    ( LitPool* , const char* , const char* );
#define LitPoolGetFalse(POOL) 0
#define LitPoolGetTrue(POOL)  1
PAPI const Lit* LitPoolIndex  ( LitPool* , LitIdx );

PAPI int StrToI32( const char* , int , int32_t* );
PAPI int StrToDbl( const char* , double* );

PAPI const char* ReportError( const char* ,
                              const char* ,
                              size_t ,
                              size_t ,
                              size_t ,
                              const char* );

#define ALIGN(XX,A) (((XX) + ((A)-1)) & ~((A)-1))

/* -------------------------------------------------------
 * Lexer
 * ------------------------------------------------------*/
#define TOKEN_LIST(XX)          \
  XX(TK_ADD,"+")                \
  XX(TK_SUB,"-")                \
  XX(TK_MUL,"*")                \
  XX(TK_DIV,"/")                \
  XX(TK_MOD,"%")                \
  XX(TK_INC,"++")               \
  XX(TK_DEC,"--")               \
  XX(TK_LT,"<")                 \
  XX(TK_LE,"<=")                \
  XX(TK_GT,">")                 \
  XX(TK_GE,">=")                \
  XX(TK_EQ,"==")                \
  XX(TK_NE,"!=")                \
  XX(TK_AND,"&&")               \
  XX(TK_OR ,"||")               \
  XX(TK_NOT,"!" )               \
  XX(TK_DOT,"." )               \
  XX(TK_COMMA,",")              \
  XX(TK_SEMICOLON,";")          \
  XX(TK_QUESTION,"?")           \
  XX(TK_COLON,":")              \
  XX(TK_ADDR ,"&")              \
  XX(TK_LPAR ,"(")              \
  XX(TK_RPAR ,")")              \
  XX(TK_LBRA ,"{")              \
  XX(TK_RBRA ,"}")              \
  XX(TK_LSQR ,"[")              \
  XX(TK_RSQR ,"]")              \
  XX(TK_ASSIGN,"=")             \
  XX(TK_SADD  ,"+=")            \
  XX(TK_SSUB  ,"-=")            \
  XX(TK_SMUL  ,"*=")            \
  XX(TK_SDIV  ,"/=")            \
  XX(TK_SMOD  ,"%=")            \
  XX(TK_LIT_INT  ,"<int>")      \
  XX(TK_LIT_DBL  ,"<double>")   \
  XX(TK_LIT_TRUE ,"<true>")     \
  XX(TK_LIT_FALSE,"<false>")    \
  XX(TK_LIT_CHAR ,"<char>")     \
  XX(TK_LIT_STR  ,"<str>" )     \
  XX(TK_INT      ,"int"   )     \
  XX(TK_DBL      ,"double")     \
  XX(TK_BOOL     ,"bool"  )     \
  XX(TK_VOID     ,"void"  )     \
  XX(TK_CHAR     ,"char"  )     \
  XX(TK_STR      ,"string")     \
  XX(TK_ID       ,"{id}"  )     \
  XX(TK_IF       ,"if"    )     \
  XX(TK_ELIF     ,"elif")       \
  XX(TK_ELSE     ,"else"  )     \
  XX(TK_FOR      ,"for"   )     \
  XX(TK_RETURN   ,"return")     \
  XX(TK_ERROR    ,"[error]")    \
  XX(TK_EOF      ,"[eof]"  )

typedef enum _Token {
#define XX(A,B) A,
  TOKEN_LIST(XX)
#undef XX // XX
  SIZE_OF_TOKENS
} Token;

PAPI const char* TokenGetStr( Token );

typedef struct _Lexeme {
  LitIdx lit;
  Token tk;
  size_t tk_sz;
} Lexeme;

typedef struct _Lexer {
  const char* src;
  size_t      pos;
  Lexeme      lexeme;
  size_t      nline;
  size_t      nchar;
  LitPool*    lpool;
  const char*   err;
} Lexer;

PAPI void LexerInit( Lexer* , LitPool* , const char* );
PAPI const Lexeme* LexerNext( Lexer* );
PAPI void LexerDelete( Lexer* );

/* -------------------------------------------------------
 * Type
 * ------------------------------------------------------*/

typedef enum _EType {
  ET_STR,
  ET_ARR,
  ET_STRUCT,
  ET_FUNC,

  // primitive type
  EPT_INT,
  EPT_DBL,
  EPT_CHAR,
  EPT_BOOL,
  EPT_VOID,

  ET_UNKNOWN
} EType;

// represents a Type object
typedef struct _Type {
  struct _Type* next;
  EType          tag;
  size_t        size;
} Type;

typedef struct _PrimitiveType {
  Type     base;
} PrimitiveType;

typedef struct _StrType {
  Type base;
} StrType;

typedef struct _FieldType {
  struct _StructType* p;
  size_t         offset;   // offset from the start of the field, include padding
  const Type*         t;   // actual type for this field
  LitIdx           name;   // lit index to the name of the field type
} FieldType;

typedef struct _StructType {
  Type               base;
  const FieldType* fstart;
  size_t            fsize;
  size_t             fcap;
  LitIdx             name;
} StructType;

typedef struct _ArrType {
  Type        base;
  const Type* type;
  size_t       len;
} ArrType;

typedef struct _FuncTypeArg {
  const Type* type;
  LitIdx      name;
} FuncTypeArg;

typedef struct _FuncType {
  Type       base;
  LitIdx     name;
  const Type* ret;

  FuncTypeArg* arg;
  size_t  arg_size;
  size_t   arg_cap;
} FuncType;

typedef struct _TypeSys {
  Type*         types;
  PrimitiveType t_int;
  PrimitiveType t_dbl;
  PrimitiveType t_char;
  PrimitiveType t_bool;
  PrimitiveType t_void;
  StrType       t_str;
  MPool          pool;
  LitPool*      lpool;
} TypeSys;

PAPI
void TypeSysInit( TypeSys* , LitPool* );

PAPI
void TypeSysDelete( TypeSys* );

// getter
#define TypeSysGetInt(TS)  (const PrimitiveType*)(&((TS)->t_int))
#define TypeSysGetDbl(TS)  (const PrimitiveType*)(&((TS)->t_dbl))
#define TypeSysGetBool(TS) (const PrimitiveType*)(&((TS)->t_bool))
#define TypeSysGetChar(TS) (const PrimitiveType*)(&((TS)->t_char))
#define TypeSysGetVoid(TS) (const PrimitiveType*)(&((TS)->t_void))
#define TypeSysGetStr(TS)  (const StrType*)(&((TS)->t_str))

PAPI
const StructType* TypeSysGetStruct( TypeSys* , LitIdx );

PAPI
const ArrType* TypeSysGetArr   ( TypeSys* , const Type* , size_t length );

PAPI
const FuncType* TypeSysGetFunc  ( TypeSys* , LitIdx );

// setter
// struct type
PAPI
StructType* TypeSysSetStruct  ( TypeSys* , LitIdx );

PAPI
const FieldType* TypeSysAddStructField( TypeSys* , StructType* , LitIdx , const Type* );

// array type
PAPI
ArrType* TypeSysSetArr ( TypeSys* , const Type* , size_t length );

// func type
PAPI
FuncType* TypeSysSetFunc( TypeSys* , LitIdx );

PAPI
void TypeSysFuncSetRet( TypeSys* , FuncType* , const Type* );

PAPI
const FuncTypeArg* TypeSysFuncAddArg( TypeSys* , FuncType* , const Type* , LitIdx );

// type conversion
PAPI
int TypeSysCanCast( TypeSys* , const Type* , const Type* );

/* -------------------------------------------------------
 * Symbol
 * ------------------------------------------------------*/

typedef enum _ESymType {
  ST_ARG,
  ST_GVAR,
  ST_LVAR,
  ST_FIELD,
  ST_UNKNOWN
} ESymType;

typedef enum _EScpType {
  SCT_GLOBAL,
  SCT_FUNC  ,
  SCT_LEXICAL,
  SCT_UNKNOWN
} EScpType;

typedef struct _SymInfo {
  LitIdx      name; // variable name
  const Type* type; // variables' type
} SymInfo;

typedef struct _Sym {
  struct _Sym* next;
  SymInfo      info;
  ESymType     type;
} Sym;

typedef struct _LVar {
  Sym base;         // symbol of the local varaibles
  size_t offset;    // offset from the frame registers
} LVar;

typedef struct _GVar {
  Sym base;         // symbol of the global variables
  size_t offset;    // offset from the global variable region
} GVar;

typedef struct _Arg {
  Sym base;
  size_t offset;
} Arg;

typedef struct _SymTable {
  Sym* sym;
  size_t sz;
} SymTable;

// Scope representation
typedef struct _Scp {
  struct _Scp* prev;  // previous scope
  EScpType     type;  // types of the scope
} Scp;

typedef struct _GlbScp {
  Scp scp;
  SymTable stb;
} GlbScp;

typedef struct _FuncScp {
  Scp scp;
  SymTable stb;
  const FuncType* type;
} FuncScp;

typedef struct _LexScp {
  Scp scp;
  SymTable stb;
  uint32_t in_loop : 1;
  uint32_t is_loop : 1;
} LexScp;

/* -------------------------------------------------------
 * Parser
 * ------------------------------------------------------*/

// expression AST definition. For expression code gen we do a
// 2 pass generation ; the 1st pass will convert text data into
// an AST representation and the 2nd pass will constant fold the
// expression if needed and then apply a relative better register
// allocator for expression generation. Rest of the statements in
// langauge will do naive code generation directly to machine code
typedef enum _EExprType {
  EET_LIT,
  EET_ID,
  EET_PREFIX,
  EET_UNARY,
  EET_BINARY,
  EET_TERANRY
} EExprType;

typedef struct _Expr {
  EExprType type;
  size_t nline;
  size_t nchar;
  size_t dbg_start;
  size_t dbg_end;
} Expr;

typedef struct _ExprLit {
  Expr    base;
  LitIdx lit;
} ExprLit;

typedef struct _ExprId {
  Expr    base;
  Sym*     sym;
} ExprId;

typedef struct _ExprStruLitAssign {
  struct _ExprStruLitAssign* next;
  const FieldType* ftype;
  Expr*            value;
} ExprStruLitAssign;

typedef struct _ExprStruLit {
  Expr base;
  const Type* ctype;          // the type of the structure literals
  ExprStruLitAssign* assign;  // all the field assignment
} ExprStruLit;

typedef struct _ExprPrefixCompCall {
  Expr* args[CONFIG_MAX_CALL_ARGS];
  size_t   sz;
} ExprPrefixCompCall;

typedef enum _EExprPrefixCompType {
  EEPCT_DOT,
  EEPCT_IDX,
  EEPCT_CALL
} EExprPrefixCompType;

typedef struct _ExprPrefixComp {
  struct _ExprPrefixComp* next;
  union {
    Sym*     dot;
    Expr* idx;
    ExprPrefixCompCall call;
  } c;
  EExprPrefixCompType comp_type;
} ExprPrefixComp;

typedef struct _ExprPrefix {
  Expr     base;
  LitIdx   init;
  ExprPrefixComp* rest;
} ExprPrefix;

typedef struct _ExprUnary {
  Expr       base;
  const Expr* opr;
  Token        op;
} ExprUnary;

typedef struct _ExprBinary {
  Expr     base;
  const Expr* lhs;
  const Expr* rhs;
  Token        op;
} ExprBinary;

typedef struct _ExprTernary {
  Expr     base;
  const Expr* first;
  const Expr* second;
  const Expr* third;
} ExprTernary;


#endif // TCMM_H_
