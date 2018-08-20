#include <xbyak/xbyak.h>
#include <fe/tcmm.h>

#include <vector>
#include <variant>

namespace {
namespace jit { using namespace Xbyak; using namespace Xbyak::util; } // namespace jit

class RegAlloc;

static const std::uint32_t kStackStorageSize = 8;

enum StorageType { BYTE , WORD , DWORD , QWORD , DBL };

// represents a slot on the stack.
class StackLoc {
 public:
  StackLoc( size_t idx , StorageType type ): index_(idx) , type_(type) {}
  StorageType type () const { return type_;  }
  int index() const { return index_; }
  std::uint32_t off() const { return static_cast<std::uint32_t>(index_) * kStackStorageSize; }
 private:
  int index_;
  StorageType type_;
};

// represents an RA result , can be in memory or inside of the register
class RARes{
  class None {};
 public:
  enum { NONE = 0 , REGISTER , STACK };

  Loc( const jit::Reg& opr ) : val_(opr) {}
  Loc( const StackLoc& s   ) : val_(s)   {}
  Loc()                      : val_      {}

  bool IsNone    () const { return val_.index() == NONE;     }
  bool IsRegister() const { return val_.index() == REGISTER; }
  bool IsStack   () const { return val_.index() == STACK   ; }

  const jit::Reg& ToRegister() const {
    assert(IsRegister());
    return std::get<jit::Reg>(val_);
  }

  const StackLoc& ToStack() const {
    assert(IsStack());
    return std::get<StackLoc>(val_);
  }

  // get the storage type related to this Loc object
  StorageType storage_type() const;
 private:
  std::variant<None,jit::Reg,StackLoc> val_;
};

class Loc {
 public:
  Loc( const jit::Reg& opr , RegAlloc* ra ):
    res_(opr),
    init_(true),
    ra_  (ra)
  {}

  Loc( const StackLoc& s , RegAlloc* ra ) :
    res_(s),
    init_(true),
    ra_(ra)
  {}

  Loc()             : res_() , init_(false) , ra_ (that.ra_) {}

  Loc( Loc&& that ) : res_(std::move(that.res_)) , init_(that.init_) , ra_(that.ra_) {}
  Loc& operator = ( Loc&& that ) {
    res_ = that.res_;
    init_= that.init_;
    ra_  = that.ra_;

    that.init_ = false; // ownership transferred
    return *this;
  }

  ~Loc();
 public:
  bool IsNone    () const { assert(init_); return res_.IsNone(); }
  bool IsRegister() const { assert(init_); return res_.IsRegister(); }
  bool IsStack   () const { assert(init_); return res_.IsStack();    }
  const jit::Reg& ToRegister() const { assert(init_); return res_.ToRegister(); }
  const StackLoc& ToStack   () const { assert(init_); return res_.ToStack();    }
  StorageType storage_type() const { assert(init_); return res_.storage_type(); }

 public:
  bool  IsInit() const { return init_; }
  const RARes& res () const { return res_;  }

  void Reset();
 private:
  RARes res_;
  bool  init_;
  RegAlloc* ra_;
  // don't allow copy and assign the only way to work with it is *move*
  Loc( const Loc& ) = delete;
  Loc& operator = ( const Loc& ) = delete;
};

// a simple register allocator, the register just use greedy algorithm to
// do allocation, it tries to allocate variable into register as much as
// it can and then once the register used up fallback to stack. It doesn't
// do spill since spill requires liveness analysis which we don't have it
// right now. We can do liveness analysis on top of the AST but better do
// it on CFG in backwards order
class RegAlloc {
 public:
  enum { RAX = 0 , RCX , RDX , RBX , RSP , RBP , RSI , RDI ,
         R8      , R9  , R10 , R11 ,
         /** r12 - r15 is not used since it is callee saved reigsters in C abi,
          *  and we don't use them here at all to prevent to much register preserve
          *  during the function prolog. The reigster allocator cannot do spill
          **/

         INT_REGISTER,

         /** xmm registers used for floating point operation, they are all caller
          *  saved registers so very safe to use betwee C frame and our internal
          *  frame
          **/
         XMM0    , XMM1, XMM2, XMM3, XMM4, XMM5, XMM6, XMM7,
         XMM8    , XMM9, XMM10,XMM11,XMM12,XMM13,XMM14,XMM15,

         DBL_REGISTER,

         SIZE_OF_REGISTERS };

  RegAlloc();
 public:
  Loc  Grab( StorageType );

  void Free( const RARes& );

 public: // primitive way to do register manipulation, should not use it but prefer Grab
  void Lock  ( int idx ) { reg_status_[idx] = true; }
  void Unlock( int idx ) { reg_status_[idx] = false; }
 private:
  Loc  NewReg( int , StorageType );

  int  MapRegIndex( const jit::Reg& opr );

 private:

  std::vector<bool> reg_status_; // register status
  std::vector<bool> stk_;
};

void Loc::Reset() { if(init_) { init_ = false; ra_->Free(this.res_); }

Loc::~Loc() { Reset(); }

RegAlloc::RegAlloc(): reg_status_(), stk_() {
  reg_status_.resize( SIZE_OF_REGISTERS );
  reg_status_[RBX] = true; // context
  reg_status_[RSP] = true; // stack
  reg_status_[RBP] = true; // frame pointer

  /** scratch register used as temporary storage **/
  reg_status_[RAX] = true;
  reg_status_[RCX] = true;
  reg_status_[XMM0]= true;
}

Loc RegAlloc::Grab( StorageType st ) {

  // tries to scan through the register sets
  {
    size_t start;
    size_t end  ;
    if(st == DBL) { start = XMM0 ; end = DBL_REGISTER; }
    else          { start = 0    ; end = INT_REGISTER; }
    for( ; start < end ; ++start ) {
      if(!reg_status_[start]) {
        reg_status_[start] = true;
        return Loc(NewReg(static_cast<int>(start),st),this);
      }
    }
  }

  // tries to allocate it from the *stack*
  for( std::size_t i = 0 ; i < stk_.size() ; ++i ) {
    if(!stk_[i]) {
      stk_[i] = true;
      return Loc(StackLoc(i,st),this);
    }
  }

  // cannot find it from the stk
  stk_.push_back(true);
  return Loc(StackLoc(stk_.size()-1,st),this);
}

void RegAlloc::Free( const RARes& l ) {
  if(l.IsRegister()) {
    auto &r = l.ToRegister();
    auto idx= MapRegIndex(r);
    assert(reg_status_[idx]);
    reg_status_[idx] = false;
  } else {
    auto &s= l.ToStack();
    assert(stk_[s.index]);
    stk_[s.index()] = false;
    while( !stk_.empty() && !stk_.back() ) { stk_.pop_back(); }
  }
}

jit::Reg RegAlloc::NewReg( int idx , StorageType st ) {

  switch(idx) {

#define DO(B,W,DW,QW)             \
  do {                            \
    switch(st) {                  \
      case BYTE: return (B);      \
      case WORD: return (W);      \
      case DWORD:return (DW);     \
      case QWORD:return (QW);     \
      default: assert(0); break;  \
    }                             \
    return (B);                   \
  } while(false)

    case RAX: DO(jit::al,jit::ax,jit::eax,jit::rax);
    case RBX: DO(jit::bl,jit::bx,jit::ebx,jit::rbx);
    case RCX: DO(jit::cl,jit::cx,jit::ecx,jit::rcx);
    case RDX: DO(jit::dl,jit::dx,jit::edx,jit::rdx);
    case RSI: DO(jit::sil,jit::si,jit::esi,jit::rsi);
    case RDI: DO(jit::dil,jit::di,jit::edi,jit::rdi);
    case RBP: DO(jit::bpl,jit::bp,jit::ebp,jit::rbp);
    case RSP: DO(jit::spl,jit::sp,jit::esp,jit::rsp);
    case R8:  DO(jit::r8b,jit::r8w,jit::r8d,jit::r8);
    case R9:  DO(jit::r9b,jit::r9w,jit::r9d,jit::r9);
    case R10: DO(jit::r10b,jit::r10w,jit::r10d,jit::r10);
    case R11: DO(jit::r11b,jit::r11w,jit::r11d,jit::r11);

#undef DO // DO


#define DO(B,W,DW,QW)            \
  do {                           \
    if(st == DBL) return (QW);   \
    assert(0);                   \
    return (B);                  \
  } while(false)

    case XMM0:  DO(_,_,_,jit::xmm0);
    case XMM1:  DO(_,_,_,jit::xmm1);
    case XMM2:  DO(_,_,_,jit::xmm2);
    case XMM3:  DO(_,_,_,jit::xmm3);
    case XMM4:  DO(_,_,_,jit::xmm4);
    case XMM5:  DO(_,_,_,jit::xmm5);
    case XMM6:  DO(_,_,_,jit::xmm6);
    case XMM7:  DO(_,_,_,jit::xmm7);

    case XMM8 : DO(_,_,_,jit::xmm8);
    case XMM9 : DO(_,_,_,jit::xmm9);
    case XMM10: DO(_,_,_,jit::xmm10);
    case XMM11: DO(_,_,_,jit::xmm11);
    case XMM12: DO(_,_,_,jit::xmm12);
    case XMM13: DO(_,_,_,jit::xmm13);
    case XMM14: DO(_,_,_,jit::xmm14);
    case XMM15: DO(_,_,_,jit::xmm15);

#undef DO // DO
    default: assert(0);
  }

  // unreachable
  return jit::rax;
}

class X64CGen {
 public:
 private:
  std::uint32_t temp_base() const { return local_var_offset_; }
  std::uint32_t arg_off  ( size_t ref ) const;
  void*         dbl_table() const;

 private: // helpers
  const jit::Reg& ToTmp( const Loc& , const jit::Reg& );
 private:
  /** expression generation --------------------------- **/
  Loc GenExpr   ( const Node*        );
  Loc GenLit    ( const NodeLit*     );
  Loc GenId     ( const NodeId*      );
  Loc GenDot    ( const NodeDot*     );
  Loc GenIndex  ( const NodeIndex*   );
  Loc GenCall   ( const NodeCall*    );
  Loc GenUnary  ( const NodeUnary*   );
  Loc GenBinary ( const NodeBinary*  );
  Loc GenTernary( const NodeTernary* );


 private:
  jit::CodeGenerator gen_;

  const jit::Reg&    ctx_;
};

#define __ ((&gen_)->)

Loc X64CGen::GenExpr( const Node* n ) {
  switch(n->type) {
    case ENT_LIT:    return GenLit((const NodeLit*)n);
    case ENT_ID:     return GenId ((const NodeId* )n);
    case ENT_DOT:    return GenDot((const NodeDot*)n);
    case ENT_INDEX:  return GenIndex((const NodeIndex*)n);
    case ENT_CLAL:   return GenCall((const NodeCall*)n);
    case ENT_UNARY:  return GenUnary((const NodeUnary*)n);
    case ENT_BINARY: return GenBinary((const NodeBinary*)n);
    case ENT_TERNARY:return GenTernary((const NodeTernary*)n);
    default: assert(0);
  }
}

const jit::Reg& X64CGen::ToTmp( const Loc& l , const jit::Reg& r ) {
  if(l.IsRegister())
    return l.ToRegister();
  else {
    const &ss = l.ToStack();
    __ mov(r,qword [ jit::rsp + ss.off() ]);
    return r;
  }
}

Loc X64CGen::GenIndex(const NodeIndex* n) {
  const Type* ctype = n->ctype;
  auto lhs_loc = GenExpr(n->lhs);
  auto rhs_loc = GenExpr(n->rhs);


  if(ctype->type == ET_ARR) {
    auto lreg = ToTmp(lhs_loc,jit::rax);
    auto rreg = ToTmp(rhs_loc,jit::rcx);
    Loc  ret;

  } else {
  }

}

Loc X64CGen::GenDot(const NodeDot* n) {
  auto lhs_loc   = GenExpr(n->lhs);
  const FieldType* field = n->ctype;
  const Type*         ft = field->t;
  Loc reg;

  jit::Reg lreg;

  if(lhs_loc.IsRegister()) {
    lreg = lhs_loc.ToRegister();
  } else {
    const &ss = lhs_loc.ToStack();
    __ mov ( jit::rax , qword [ jit::rsp + ss.off() ] );
  }

  // lreg has pointer points to the structure on heap
  switch(ft->type) {
    case EPT_INT:
      reg = ra_.Grab(DWORD);
      if(reg.IsRegister())
        __ mov ( reg.ToRegister() , qword[lreg + field->offset] );
      else {
        __ mov ( jit::rax , qword[lreg + field->offset] );
        __ mov ( qword [jit::rsp + reg.off()] , jit::rax );
      }
      break;
    case EPT_DBL:
      reg = ra_.Grab(DBL);
      if(reg.IsRegister()) {
        assert(reg.ToRegister().isXMM());
        __ movsd (reg.ToRegister(),qword[lreg + field->offset]);
      } else {
        __ movsd(jit::rax , qword[lreg + field->offset]);
        __ movd (reg.ToRegister(),jit::rax);
      }
      break;
    case EPT_BOOL:
    case EPT_CHAR:
      reg = ra_.Grab(BYTE);
      __ mov ( reg.ToRegister() , byte[lreg + field->offset] );
      break;
    /** both are reference types **/
    case EPT_STR:
    case EPT_ARR:
    case EPT_STRUCT:
      reg = ra_.Grab(QWORD);
      __ mov ( reg.ToRegister() , qword[lreg + field->offset] );
      break;
    default:
      assert(0);
      break;
  }

  return reg;
}

Loc X64CGen::GenId(const NodeId* n) {
  const Type* ctype = n->ctype;

  switch(n->ref.ref_type) {
    case EIRT_GLOBAL:
      return GenGlobal(n->name);
    case EIRT_LOCAL:
      {
        Loc ret;
        if(ctype->type == EPT_DBL) {
          ret = ra_.Grab(DBL);
          if(ret.IsRegister())
            __ movsd (ret.ToRegister(),qword [jit::rbp + n->ref.d.slots * kStackStorageSize]);
          else {
            auto &ss = ret.ToStack();
            __ movsd (jit::xmm0,qword [jit::rbp + n->ref.d.slots * kStackStorageSize]);
            __ movsd (qword [jit::rsp + ss.off()] , jit::xmm0);
          }
        } else {
          ret = ra_.Grab(QWORD);
          if(ret.IsRegister()) {
            __ mov (ret.ToRegister(),qword [jit::rbp + n->ref.d.slots * kStackStorageSize]);
          } else {
            auto &ss = ret.ToStack();
            __ mov (jit::rax, qword [jit::rbp + n->ref.d.slots * kStackStorageSize]);
            __ mov (qword [ jit::rsp + ss.off() ], jit::rax );
          }
        }
        return ret;
      }
    case EIRT_ARG:
      {
        Loc ret;
        auto idx = n->ref.d.slots;

        if(ctype->type == EPT_DBL) {
          ret = ra_.Grab(DBL);
          if(ret.IsRegister()) {
            __ movsd (ret.ToRegister(),qword [jit::rbp - arg_off(idx)]);
          } else {
            auto &ss = ret.ToStack();
            __ movsd (jit::xmm0,qword [jit::rbp - arg_off(idx)]);
            __ movsd (qword [jit::rsp + ss.off()] , jit::xmm0);
          }
        } else {
          ret = ra_.Grab(QWORD);
          if(ret.IsRegister()) {
            __ mov (ret.ToRegister(),qword[jit::rbp - arg_off(idx)]);
          } else {
            auto &ss = ret.ToStack();
            __ mov (jit::rax, qword [jit::rbp - arg_off(idx)]);
            __ mov (qword [ jit::rsp + ss.off() ], jit::rax);
          }
        }
        return ret;
      }
    default: assert(0); break;
  }

  return Loc();
}

Loc X64CGen::GenLit(const NodeLit* n) {
  switch(n->ctype->type) {
    case EPT_INT:
      {
        int  iv = LitPoolInt (lit_pool_,n->lit);
        auto ret= ra_.Grab   (DWORD);

        if(ret.IsRegister()) {
          if(!bv)
            __ xor_(ret.ToRegister(),ret.ToRegister());
          else
            __ mov (ret.ToRegister(),1);
        } else {
          auto &ss = ret.ToStack();
          if(!bv)
            __ xor_(jit::rax,jit::rax);
          else
            __ mov (jit::rax,1);

          __ mov(dword [jit::rsp + ss.off()], jit::rax);
        }
        return ret;
      }
    case EPT_CHAR:
    case EPT_INT:
      {
        int iv;
        switch(c->ctype->type) {
          case EPT_BOOL: iv = LitPoolBool(lit_pool_,n->lit); break;
          case EPT_CHAR: iv = LitPoolBool(lit_pool_,n->lit); break;
          default: break;
        }

        auto ret = ra_.Grab(BYTE);
      }
      break;

    case EPT_DBL:
      {
        auto ret = ra_.Grab(DBL);
        __ mov(jit::rax,ptr [ctx_]);
        if(ret.IsRegister()) {
          __ movsd(ret.ToRegister(),qword [jit::rax + 8 * n->ref]);
        } else {
          auto &ss = ret.ToStack();

          /*
           * same as below, but prefer not using xmm registers since
           * they are longer to encode
           *
           * __ movsd(jit::xmm0,qword [jit::rax + 8 * n ->ref]);
           * __ movsd(qword [jit::rsp + ss.off()], jit::xmm0 );
           *
           */
          __ mov(jit::rax, qword [jit::rax + 8 * n->ref]);
          __ mov(qword [jit::rsp + ss.off()], jit::rax);
        }

        return ret;
      }

    case EPT_STR:
      {
        auto ret = ra_.Grab(QWORD);
        __ mov(jit::rax,ptr [ctx_ + offsetof(Ctx,str_tb)] );
        if(ret.IsRegister()) {
          __ mov (ret.ToRegister(),ptr [jit::rax + 8 * n->ref]);
        } else {
          auto &ss = ret.ToStack();
          __ mov (jit::rax,qword [jit::rax + 8 * n->ref]);
          __ mov (qword [jit::rsp + ss.off()], jit::rax);
        }
      }
      return ret;

    default: assert(0); break;
  }

  return Loc();
}


















