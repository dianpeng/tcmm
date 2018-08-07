#ifndef TCMM_H_
#define TCMM_H_
#include <stdio.h>
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
  MPool* mpool;
} LitPool;

typedef size_t LitIdx;

PAPI void   LitPoolInit     ( LitPool* , MPool* );
PAPI void   LitPoolDelete   ( LitPool* );
PAPI LitIdx LitPoolGetDouble( LitPool* , double );
PAPI LitIdx LitPoolGetInt   ( LitPool* , int32_t);
PAPI LitIdx LitPoolGetChar  ( LitPool* , char   );
PAPI LitIdx LitPoolGetStr   ( LitPool* , const char* );
PAPI LitIdx LitPoolGetId    ( LitPool* , const char* , const char* );
#define LitPoolGetFalse(POOL) 0
#define LitPoolGetTrue(POOL)  1
PAPI const Lit* LitPoolIndex  ( LitPool* , LitIdx );

PAPI const char* LitPoolStr ( LitPool* , LitIdx );
PAPI const char* LitPoolId  ( LitPool* , LitIdx );
PAPI double      LitPoolDbl ( LitPool* , LitIdx );
PAPI int32_t     LitPoolInt ( LitPool* , LitIdx );
PAPI char        LitPoolChar( LitPool* , LitIdx );
PAPI int         LitPoolBool( LitPool* , LitIdx );

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
  XX(TK_BREAK    ,"break" )     \
  XX(TK_CONTINUE ,"continue")   \
  XX(TK_RETURN   ,"return")     \
  XX(TK_STRUCT   ,"struct")     \
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

PAPI int LexerIsEscChar( int );
PAPI int LexerEscChar  ( int );

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
  size_t       align;
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
  FieldType*       fstart;
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
  MPool*         pool;
  LitPool*      lpool;
} TypeSys;

PAPI
void TypeSysInit( TypeSys* , LitPool* , MPool* );

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
const FieldType* TypeSysGetStructField( TypeSys* , const StructType* , LitIdx );

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
  ST_DEFINE,
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
  SymInfo      info;
  ESymType     type;
} Sym;

typedef struct _LVar {
  Sym      base;    // symbol of the local varaibles
  size_t offset;    // offset from the frame registers
} LVar;

typedef struct _GVar {
  Sym      base;    // symbol of the global variables
  size_t offset;    // offset from the global variable region
} GVar;

typedef struct _Arg {
  Sym      base;
  size_t offset;
} Arg;

typedef struct _Define {
  Sym base;
} Define;

typedef struct _SymTable {
  Sym**   sym;
  size_t   sz;
  size_t  cap;
  MPool*  pool;
} SymTable;

PAPI void SymTableInit  ( SymTable* , MPool* );
PAPI void SymTableDelete( SymTable* );
PAPI const LVar*   SymTableGetLVar  ( SymTable* , LitIdx );
PAPI const GVar*   SymTableGetGVar  ( SymTable* , LitIdx );
PAPI const Arg*    SymTableGetArg   ( SymTable* , LitIdx );
PAPI const Define* SymTableGetDefine( SymTable* , LitIdx );
PAPI LVar*         SymTableSetLVar  ( SymTable* , LitIdx , const Type* );
PAPI GVar*         SymTableSetGVar  ( SymTable* , LitIdx , const Type* );
PAPI Arg*          SymTableSetArg   ( SymTable* , LitIdx , const Type* );
PAPI Define*       SymTableSetDefine( SymTable* , LitIdx , const Type* );

// Scope representation
typedef struct _Scp {
  struct _Scp* prev;  // previous scope
  EScpType     type;  // types of the scope
} Scp;

typedef struct _GlbScp {
  Scp     base;
  SymTable stb;
} GlbScp;

typedef struct _FuncScp {
  Scp             base;
  SymTable         stb;
  const FuncType* type;
  size_t     max_stksz; // maximum stack size
} FuncScp;

typedef struct _LexScp {
  Scp      base;
  SymTable stb;
  size_t   vsz;         // all variable accumulated size that is nested up to now
  uint32_t in_loop : 1;
  uint32_t is_loop : 1;
} LexScp;

/* -------------------------------------------------------
 * Parser
 * ------------------------------------------------------*/

typedef enum _ENodeType {
  // expression
  ENT_LIT,
  ENT_ID,
  ENT_STRULIT,
  ENT_PREFIX,
  ENT_UNARY,
  ENT_BINARY,
  ENT_TERNARY
} ENodeType;

typedef struct _Node {
  ENodeType type;
  size_t dbg_start;
  size_t dbg_end;
} Node;

typedef struct _NodeLit {
  Node    base;
  LitIdx   lit;
} NodeLit;

typedef struct _NodeId {
  Node    base;
  LitIdx  name;
} NodeId;

typedef struct _NodeStruLitAssign {
  struct _NodeStruLitAssign* next;
  const FieldType* ftype;
  Node*            value;
} NodeStruLitAssign;

typedef struct _NodeStruLit {
  Node base;
  const StructType* ctype;    // the type of the structure literals
  NodeStruLitAssign* assign;  // all the field assignment
} NodeStruLit;

typedef struct _NodePrefixCompCall {
  Node* args[CONFIG_MAX_CALL_ARGS];
  size_t   sz;
  size_t   dbg_start;
  size_t   dbg_end;
} NodePrefixCompCall;

typedef enum _ENodePrefixCompType {
  EEPCT_DOT,
  EEPCT_IDX,
  EEPCT_CALL
} ENodePrefixCompType;

typedef struct _NodePrefixComp {
  union {
    LitIdx  name;
    Node*    idx;
    NodePrefixCompCall* call;
  } c;
  ENodePrefixCompType comp_type;
} NodePrefixComp;

typedef struct _NodePrefix {
  Node            base;
  LitIdx          init;
  NodePrefixComp* comp;
  size_t          comp_sz;
  size_t          comp_cap;
} NodePrefix;

typedef struct _NodeUnary {
  Node       base;
  const Node* opr;
  Token        op;
} NodeUnary;

typedef struct _NodeBinary {
  Node     base;
  const Node* lhs;
  const Node* rhs;
  Token        op;
} NodeBinary;

typedef struct _NodeTernary {
  Node     base;
  const Node* first;
  const Node* second;
  const Node* third;
} NodeTernary;

/** statements **/
typedef struct _NodeChunk {
  Node** stmt;
  size_t   sz;
  size_t  cap;
} NodeChunk;

typedef NodePrefix NodeCall;

typedef struct _NodeLocal {
  Node base;
  LitIdx name;
  Node*   rhs;
} NodeLocal;

typedef NodePrefix NodeAssignLHS;

typedef struct _NodeAssign {
  Node base;
  NodeAssignLHS* lhs;
  Node*          rhs;
  int op;
} NodeAssign;

typedef struct _NodeIfBranch {
  Node* cond;
  NodeChunk* chunk;
} NodeIfBranch;

typedef struct _NodeIf {
  Node            base;
  NodeIfBranch* chains;
  size_t            sz;
  size_t           cap;
} NodeIf;

typedef struct _NodeFor {
  Node            base;
  Node*           init;
  Node*           cond;
  Node*           step;
} NodeFor;

typedef struct _NodeBreak {
  Node            base;
} NodeBreak;

typedef struct _NodeContinue {
  Node            base;
} NodeContinue;

typedef struct _NodeReturn {
  Node            base;
  Node*           expr;
} NodeReturn;

typedef struct _NodeFunction {
  Node            base;
  const FuncType* type;
  NodeChunk*     chunk;
} NodeFunction;

typedef struct _Parser {
  LitPool* lpool;
  TypeSys*  tsys;
  Lexer    lexer;
  MPool     pool;

  const char* err;
} Parser;

PAPI NodePrefixComp* NodePrefixAddComp( NodePrefix* p , MPool* );
PAPI void NodeToJSON( Parser* , FILE* , const Node* );

PAPI void ParserInit( Parser* , LitPool* , TypeSys* , const char* );
PAPI void ParserDelete( Parser* );

#endif // TCMM_H_
