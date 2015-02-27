/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef jit_mips64_Architecture_mips64_h
#define jit_mips64_Architecture_mips64_h

#include "mozilla/MathAlgorithms.h"

#include <limits.h>
#include <stdint.h>

#include "js/Utility.h"

// gcc appears to use _mips_hard_float to denote
// that the target is a hard-float target.
#ifdef _mips_hard_float
#define JS_CODEGEN_MIPS64_HARDFP
#endif

#if _MIPS_SIM == _ABI64
#define USES_N64_ABI
#elif _MIPS_SIM == _ABIN32
#define USES_N32_ABI
#else
#error "Unsupported ABI"
#endif

namespace js {
namespace jit {

// Shadow stack space is not required on MIPS64.
static const uint32_t ShadowStackSpace = 0;

// Size of each bailout table entry.
// For MIPS this is 2 instructions relative call.
static const uint32_t BAILOUT_TABLE_ENTRY_SIZE = 2 * sizeof(void *);

// On MIPS64, general purpose register is 64-bit, pass the first
// eight arguments through registers (a0-a7). In this case, we just using
// the low 32-bits of GPRs in JavaScript engine with JS_NUNBOX32.
class Registers
{
  public:
    enum RegisterID {
        r0 = 0,
        r1,
        r2,
        r3,
        r4,
        r5,
        r6,
        r7,
        r8,
        r9,
        r10,
        r11,
        r12,
        r13,
        r14,
        r15,
        r16,
        r17,
        r18,
        r19,
        r20,
        r21,
        r22,
        r23,
        r24,
        r25,
        r26,
        r27,
        r28,
        r29,
        r30,
        r31,
        zero = r0,
        at = r1,
        v0 = r2,
        v1 = r3,
        a0 = r4,
        a1 = r5,
        a2 = r6,
        a3 = r7,
        a4 = r8,
        a5 = r9,
        a6 = r10,
        a7 = r11,
        t0 = r12,
        t1 = r13,
        t2 = r14,
        t3 = r15,
        s0 = r16,
        s1 = r17,
        s2 = r18,
        s3 = r19,
        s4 = r20,
        s5 = r21,
        s6 = r22,
        s7 = r23,
        t8 = r24,
        t9 = r25,
        k0 = r26,
        k1 = r27,
        gp = r28,
        sp = r29,
        fp = r30,
        ra = r31,
        invalid_reg
    };
    typedef RegisterID Code;

    static const char *GetName(Code code) {
        static const char * const Names[] = { "zero", "at", "v0", "v1", "a0", "a1", "a2", "a3",
                                              "a4",   "a5", "a6", "a7", "t0", "t1", "t2", "t3",
                                              "s0",   "s1", "s2", "s3", "s4", "s5", "s6", "s7",
                                              "t8",   "t9", "k0", "k1", "gp", "sp", "fp", "ra"};
        return Names[code];
    }
    static const char *GetName(uint32_t i) {
        MOZ_ASSERT(i < Total);
        return GetName(Code(i));
    }

    static Code FromName(const char *name);

    static const Code StackPointer = sp;
    static const Code Invalid = invalid_reg;

    static const uint32_t Total = 32;
    static const uint32_t Allocatable = 14;

    static const uint32_t AllMask = 0xffffffff;
    static const uint32_t ArgRegMask =
        (1 << a0) | (1 << a1) | (1 << a2) | (1 << a3) |
        (1 << a4) | (1 << a5) | (1 << a6) | (1 << a7);

    static const uint32_t VolatileMask =
        (1 << Registers::v0) |
        (1 << Registers::v1) |
        (1 << Registers::a0) |
        (1 << Registers::a1) |
        (1 << Registers::a2) |
        (1 << Registers::a3) |
        (1 << Registers::a4) |
        (1 << Registers::a5) |
        (1 << Registers::a6) |
        (1 << Registers::a7) |
        (1 << Registers::t0) |
        (1 << Registers::t1) |
        (1 << Registers::t2) |
        (1 << Registers::t3);

    // We use this constant to save registers when entering functions. This
    // is why $ra is added here even though it is not "Non Volatile".
    static const uint32_t NonVolatileMask =
        (1 << Registers::s0) |
        (1 << Registers::s1) |
        (1 << Registers::s2) |
        (1 << Registers::s3) |
        (1 << Registers::s4) |
        (1 << Registers::s5) |
        (1 << Registers::s6) |
        (1 << Registers::s7) |
        (1 << Registers::ra);

    static const uint32_t WrapperMask =
        VolatileMask |         // = arguments
        (1 << Registers::t0) | // = outReg
        (1 << Registers::t1);  // = argBase

    static const uint32_t NonAllocatableMask =
        (1 << Registers::zero) |
        (1 << Registers::at) | // at = scratch
        (1 << Registers::t8) | // t8 = scratch
        (1 << Registers::t9) | // t9 = scratch
        (1 << Registers::k0) |
        (1 << Registers::k1) |
        (1 << Registers::gp) |
        (1 << Registers::sp) |
        (1 << Registers::fp) |
        (1 << Registers::ra);

    // Registers that can be allocated without being saved, generally.
    static const uint32_t TempMask = VolatileMask & ~NonAllocatableMask;

    // Registers returned from a JS -> JS call.
    static const uint32_t JSCallMask =
        (1 << Registers::a2) |
        (1 << Registers::a3);

    // Registers returned from a JS -> C call.
    static const uint32_t CallMask =
        (1 << Registers::v0) |
        (1 << Registers::v1);  // used for double-size returns

    static const uint32_t AllocatableMask = AllMask & ~NonAllocatableMask;

    typedef uint32_t SetType;
    static uint32_t SetSize(SetType x) {
        static_assert(sizeof(SetType) == 4, "SetType must be 32 bits");
        return mozilla::CountPopulation32(x);
    }
    static uint32_t FirstBit(SetType x) {
        return mozilla::CountTrailingZeroes32(x);
    }
    static uint32_t LastBit(SetType x) {
        return 31 - mozilla::CountLeadingZeroes32(x);
    }
};

// Smallest integer type that can hold a register bitmask.
typedef uint32_t PackedRegisterMask;


// MIPS64 have 64 bit floating-point coprocessor. There are 32 double
// precision register which can also be used as single precision registers.
class FloatRegisters
{
  public:
    enum FPRegisterID {
        f0 = 0,
        f1,
        f2,
        f3,
        f4,
        f5,
        f6,
        f7,
        f8,
        f9,
        f10,
        f11,
        f12,
        f13,
        f14,
        f15,
        f16,
        f17,
        f18,
        f19,
        f20,
        f21,
        f22,
        f23,
        f24,
        f25,
        f26,
        f27,
        f28,
        f29,
        f30,
        f31,
        invalid_freg
    };
    typedef FPRegisterID Code;

    static const char *GetName(Code code) {
        static const char * const Names[] = { "f0", "f1", "f2", "f3",  "f4", "f5",  "f6", "f7",
                                              "f8", "f9",  "f10", "f11", "f12", "f13",
                                              "f14", "f15", "f16", "f17", "f18", "f19",
                                              "f20", "f21", "f22", "f23", "f24", "f25",
                                              "f26", "f27", "f28", "f29", "f30", "f31"};
        return Names[code];
    }
    static const char *GetName(uint32_t i) {
        MOZ_ASSERT(i < Total);
        return GetName(Code(i % 32));
    }

    static Code FromName(const char *name);

    static const Code Invalid = invalid_freg;

    static const uint32_t Total = 32;
    static const uint32_t TotalDouble = Total;
    static const uint32_t TotalSingle = Total;
    static const uint32_t Allocatable = 30;
    // When saving all registers we only need to do is save double registers.
    static const uint32_t TotalPhys = 32;
    static const uint32_t AllMask = (1ULL << Total) - 1;
    static const uint32_t AllDoubleMask = AllMask;

    static const uint32_t NonVolatileMask =
        (1 << FloatRegisters::f20) |
        (1 << FloatRegisters::f22) |
        (1 << FloatRegisters::f24) |
        (1 << FloatRegisters::f26) |
        (1 << FloatRegisters::f28) |
        (1 << FloatRegisters::f30);

    static const uint32_t VolatileMask = AllMask & ~NonVolatileMask;

    static const uint32_t WrapperMask = VolatileMask;

    static const uint32_t NonAllocatableMask =
        // f21 and f23 are MIPS scratch float registers.
        (1 << FloatRegisters::f21) |
        (1 << FloatRegisters::f23);

    // Registers that can be allocated without being saved, generally.
    static const uint32_t TempMask = VolatileMask & ~NonAllocatableMask;

    static const uint32_t AllocatableMask = AllMask & ~NonAllocatableMask;

    typedef uint32_t SetType;
};

template <typename T>
class TypedRegisterSet;

class FloatRegister
{
  public:
    typedef FloatRegisters Codes;
    typedef Codes::Code Code;

    Code code_;

    MOZ_CONSTEXPR FloatRegister(uint32_t code)
      : code_ (Code(code))
    { }
    MOZ_CONSTEXPR FloatRegister()
      : code_(Code(FloatRegisters::invalid_freg))
    { }

    bool operator==(const FloatRegister &other) const {
        MOZ_ASSERT(!isInvalid());
        MOZ_ASSERT(!other.isInvalid());
        return code_ == other.code_;
    }
    bool equiv(const FloatRegister &other) const { return true; }
    size_t size() const { return sizeof(double); }
    bool isInvalid() const {
        return code_ == FloatRegisters::invalid_freg;
    }

    Code code() const {
        MOZ_ASSERT(!isInvalid());
        return Code(code_);
    }
    uint32_t id() const {
        return code_;
    }
    static FloatRegister FromCode(uint32_t i) {
        MOZ_ASSERT(i < FloatRegisters::Total);
        FloatRegister r = { (FloatRegisters::Code)i };
        return r;
    }

    bool volatile_() const {
        return !!((1 << code_) & FloatRegisters::VolatileMask);
    }
    const char *name() const {
        return FloatRegisters::GetName(code_);
    }
    bool operator != (const FloatRegister &other) const {
        return code_ != other.code_;
    }
    bool aliases(const FloatRegister &other) {
        return code_ == other.code_;
    }
    uint32_t numAliased() const {
        return 1;
    }
    void aliased(uint32_t aliasIdx, FloatRegister *ret) {
        MOZ_ASSERT(aliasIdx == 0);
        *ret = *this;
    }
    uint32_t numAlignedAliased() const {
        return 1;
    }
    void alignedAliased(uint32_t aliasIdx, FloatRegister *ret) {
        MOZ_ASSERT(aliasIdx == 0);
        *ret = *this;
    }
    typedef FloatRegisters::SetType SetType;
    static uint32_t SetSize(SetType x) {
        static_assert(sizeof(SetType) == 4, "SetType must be 32 bits");
        return mozilla::CountPopulation32(x);
    }
    static Code FromName(const char *name) {
        return FloatRegisters::FromName(name);
    }
    static TypedRegisterSet<FloatRegister> ReduceSetForPush(const TypedRegisterSet<FloatRegister> &s);
    static uint32_t GetSizeInBytes(const TypedRegisterSet<FloatRegister> &s);
    static uint32_t GetPushSizeInBytes(const TypedRegisterSet<FloatRegister> &s);
    uint32_t getRegisterDumpOffsetInBytes();
    static uint32_t FirstBit(SetType x) {
        return mozilla::CountTrailingZeroes32(x);
    }
    static uint32_t LastBit(SetType x) {
        return 31 - mozilla::CountLeadingZeroes32(x);
    }
};

uint32_t GetMIPSFlags();
bool hasFPU();

// MIPS64 doesn't have double registers that can NOT be treated as float32.
inline bool
hasUnaliasedDouble() {
    return false;
}

inline bool
hasMultiAlias() {
    return false;
}

// See the comments above AsmJSMappedSize in AsmJSValidate.h for more info.
// TODO: Implement this for MIPS64. Note that it requires Codegen to respect the
// offset field of AsmJSHeapAccess.
static const size_t AsmJSCheckedImmediateRange = 0;
static const size_t AsmJSImmediateRange = 0;

} // namespace jit
} // namespace js

#endif /* jit_mips64_Architecture_mips64_h */
