/* -*- Mode: C++; tab-width: 8; indent-tabs-mode: nil; c-basic-offset: 4 -*-
 * vim: set ts=8 sts=4 et sw=4 tw=99:
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. */

#ifndef jit_mips_CodeGenerator_mips_h
#define jit_mips_CodeGenerator_mips_h

#include "jit/mips/Assembler-mips.h"
#include "jit/shared/CodeGenerator-shared.h"

namespace js {
namespace jit {

class OutOfLineBailout;
class OutOfLineUndoALUOperation;
class MulNegativeZeroCheck;
class OutOfLineTableSwitch;

class OutOfLineLoadTypedArrayOutOfBounds;
class OutOfLineTruncate;

class CodeGeneratorMIPS : public CodeGeneratorShared
{
    friend class MoveResolverMIPS;

    CodeGeneratorMIPS *thisFromCtor() {
        return this;
    }

  protected:
    // Label for the common return path.
    NonAssertingLabel returnLabel_;
    NonAssertingLabel deoptLabel_;

    inline Address ToAddress(const LAllocation &a) {
        JS_ASSERT(a.isMemory());
        int32_t offset = ToStackOffset(&a);

        // The way the stack slots work, we assume that everything from
        // depth == 0 downwards is writable however, since our frame is
        // included in this, ensure that the frame gets skipped.
        if (gen->compilingAsmJS())
            offset -= AlignmentMidPrologue;

        return Address(StackPointer, offset);
    }

    inline Address ToAddress(const LAllocation *a) {
        return ToAddress(*a);
    }

    inline Operand ToOperand(const LAllocation &a) {
        if (a.isGeneralReg())
            return Operand(a.toGeneralReg()->reg());
        if (a.isFloatReg())
            return Operand(a.toFloatReg()->reg());

        JS_ASSERT(a.isMemory());
        int32_t offset = ToStackOffset(&a);

        // The way the stack slots work, we assume that everything from
        // depth == 0 downwards is writable however, since our frame is
        // included in this, ensure that the frame gets skipped.
        if (gen->compilingAsmJS())
            offset -= AlignmentMidPrologue;

        return Operand(StackPointer, offset);
    }
    inline Operand ToOperand(const LAllocation *a) {
        return ToOperand(*a);
    }
    inline Operand ToOperand(const LDefinition *def) {
        return ToOperand(def->output());
    }

    MoveOperand toMoveOperand(const LAllocation *a) const;

    // T can be Register or Imm32
    template <typename T1, typename T2>
    bool bailoutIf(T1 lhs, T2 rhs, Assembler::Condition c, LSnapshot *snapshot);

    bool bailoutFrom(Label *label, LSnapshot *snapshot);
    bool bailout(LSnapshot *snapshot);

  protected:
    bool generatePrologue();
    bool generateEpilogue();
    bool generateOutOfLineCode();

    // Emits a branch that directs control flow to the true block if |cond| is
    // true, and the false block if |cond| is false.
    template <typename T>
    void emitBranch(Register lhs, T rhs, Assembler::Condition cond,
                    MBasicBlock *mirTrue, MBasicBlock *mirFalse);
    void emitBranch(Assembler::Condition cond, MBasicBlock *ifTrue, MBasicBlock *ifFalse);

    template <typename T>
    void branchToBlock(Register lhs, T rhs, MBasicBlock *mir, Assembler::Condition cond);
    void branchToBlock(FloatRegister lhs, FloatRegister rhs, MBasicBlock *mir,
                       Assembler::DoubleCondition cond, bool isDouble);

    bool emitTableSwitchDispatch(MTableSwitch *mir, const Register &index, const Register &base);

  public:
    // Instruction visitors.
    virtual bool visitMinMaxD(LMinMaxD *ins);
    virtual bool visitAbsD(LAbsD *ins);
    virtual bool visitAbsF(LAbsF *ins);
    virtual bool visitSqrtD(LSqrtD *ins);
    virtual bool visitSqrtF(LSqrtF *ins);
    virtual bool visitAddI(LAddI *ins);
    virtual bool visitSubI(LSubI *ins);
    virtual bool visitBitNotI(LBitNotI *ins);
    virtual bool visitBitOpI(LBitOpI *ins);

    virtual bool visitMulI(LMulI *ins);

    virtual bool visitDivI(LDivI *ins);
    virtual bool visitDivPowTwoI(LDivPowTwoI *ins);
    virtual bool visitModI(LModI *ins);
    virtual bool visitModPowTwoI(LModPowTwoI *ins);
    virtual bool visitModMaskI(LModMaskI *ins);
    virtual bool visitPowHalfD(LPowHalfD *ins);
    virtual bool visitShiftI(LShiftI *ins);
    virtual bool visitUrshD(LUrshD *ins);

    virtual bool visitTestIAndBranch(LTestIAndBranch *test);
    virtual bool visitCompare(LCompare *comp);
    virtual bool visitCompareAndBranch(LCompareAndBranch *comp);
    virtual bool visitTestDAndBranch(LTestDAndBranch *test);
    virtual bool visitTestFAndBranch(LTestFAndBranch *test);
    virtual bool visitCompareD(LCompareD *comp);
    virtual bool visitCompareF(LCompareF *comp);
    virtual bool visitCompareDAndBranch(LCompareDAndBranch *comp);
    virtual bool visitCompareFAndBranch(LCompareFAndBranch *comp);
    virtual bool visitCompareB(LCompareB *lir);
    virtual bool visitCompareBAndBranch(LCompareBAndBranch *lir);
    virtual bool visitCompareV(LCompareV *lir);
    virtual bool visitCompareVAndBranch(LCompareVAndBranch *lir);
    virtual bool visitBitAndAndBranch(LBitAndAndBranch *baab);
    virtual bool visitAsmJSUInt32ToDouble(LAsmJSUInt32ToDouble *lir);
    virtual bool visitAsmJSUInt32ToFloat32(LAsmJSUInt32ToFloat32 *lir);
    virtual bool visitNotI(LNotI *ins);
    virtual bool visitNotD(LNotD *ins);
    virtual bool visitNotF(LNotF *ins);

    virtual bool visitMathD(LMathD *math);
    virtual bool visitMathF(LMathF *math);
    virtual bool visitFloor(LFloor *lir);
    virtual bool visitFloorF(LFloorF *lir);
    virtual bool visitRound(LRound *lir);
    virtual bool visitTruncateDToInt32(LTruncateDToInt32 *ins);
    virtual bool visitTruncateFToInt32(LTruncateFToInt32 *ins);

    // Out of line visitors.
    bool visitOutOfLineBailout(OutOfLineBailout *ool);
    bool visitOutOfLineTableSwitch(OutOfLineTableSwitch *ool);

  protected:
    ValueOperand ToValue(LInstruction *ins, size_t pos);
    ValueOperand ToOutValue(LInstruction *ins);
    ValueOperand ToTempValue(LInstruction *ins, size_t pos);

    // Functions for LTestVAndBranch.
    Register splitTagForTest(const ValueOperand &value);

    void storeElementTyped(const LAllocation *value, MIRType valueType, MIRType elementType,
                           const Register &elements, const LAllocation *index);

  public:
    CodeGeneratorMIPS(MIRGenerator *gen, LIRGraph *graph, MacroAssembler *masm);

  public:
    bool visitBox(LBox *box);
    bool visitBoxFloatingPoint(LBoxFloatingPoint *box);
    bool visitUnbox(LUnbox *unbox);
    bool visitValue(LValue *value);
    bool visitDouble(LDouble *ins);
    bool visitFloat32(LFloat32 *ins);

    bool visitLoadSlotV(LLoadSlotV *load);
    bool visitLoadSlotT(LLoadSlotT *load);
    bool visitStoreSlotT(LStoreSlotT *load);

    bool visitLoadElementT(LLoadElementT *load);

    bool visitGuardShape(LGuardShape *guard);
    bool visitGuardObjectType(LGuardObjectType *guard);
    bool visitGuardClass(LGuardClass *guard);
    bool visitImplicitThis(LImplicitThis *lir);

    bool visitInterruptCheck(LInterruptCheck *lir);

    bool visitNegI(LNegI *lir);
    bool visitNegD(LNegD *lir);
    bool visitNegF(LNegF *lir);
    bool visitLoadTypedArrayElementStatic(LLoadTypedArrayElementStatic *ins);
    bool visitStoreTypedArrayElementStatic(LStoreTypedArrayElementStatic *ins);
    bool visitAsmJSLoadHeap(LAsmJSLoadHeap *ins);
    bool visitAsmJSStoreHeap(LAsmJSStoreHeap *ins);
    bool visitAsmJSLoadGlobalVar(LAsmJSLoadGlobalVar *ins);
    bool visitAsmJSStoreGlobalVar(LAsmJSStoreGlobalVar *ins);
    bool visitAsmJSLoadFuncPtr(LAsmJSLoadFuncPtr *ins);
    bool visitAsmJSLoadFFIFunc(LAsmJSLoadFFIFunc *ins);

    bool visitAsmJSPassStackArg(LAsmJSPassStackArg *ins);

    bool generateInvalidateEpilogue();
  protected:
    void postAsmJSCall(LAsmJSCall *lir) {}

    bool visitEffectiveAddress(LEffectiveAddress *ins);
    bool visitUDiv(LUDiv *ins);
    bool visitUMod(LUMod *ins);
};

typedef CodeGeneratorMIPS CodeGeneratorSpecific;

// An out-of-line bailout thunk.
class OutOfLineBailout : public OutOfLineCodeBase<CodeGeneratorMIPS>
{
    LSnapshot *snapshot_;
    uint32_t frameSize_;

  public:
    OutOfLineBailout(LSnapshot *snapshot, uint32_t frameSize)
      : snapshot_(snapshot),
        frameSize_(frameSize)
    { }

    bool accept(CodeGeneratorMIPS *codegen);

    LSnapshot *snapshot() const {
        return snapshot_;
    }
};

} // namespace jit
} // namespace js

#endif /* jit_mips_CodeGenerator_mips_h */

