////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "vm.hpp"
#include "builtins.hpp"
#include "bytecode.hpp"
#include "lisp.hpp"
#include "lisp_internal.hpp"
#include "listBuilder.hpp"
#include "number/endian.hpp"


namespace lisp
{


template <typename Instruction>
Instruction* read(ScratchBuffer& buffer, int& pc)
{
    auto result = (Instruction*)(buffer.data_ + pc);
    pc += sizeof(Instruction);
    return result;
}


bool vm_can_suspend(Value*& agitant)
{
    bool can_suspend = true;
    bool seen_interpreted_context = false;
    // NOTE: stacktrace includes frames up-to, but not including, the current
    // executing function.
    auto strace = lisp::stacktrace();
    l_foreach(strace, [&](Value* v) {
        if (v->type() == Value::Type::function) {
            if (v->hdr_.mode_bits_ == Function::ModeBits::lisp_function) {
                seen_interpreted_context = true;
            }
            if ((seen_interpreted_context and
                 v->hdr_.mode_bits_ == Function::ModeBits::lisp_bytecode_function) or
                v->hdr_.mode_bits_ == Function::ModeBits::cpp_function) {
                agitant = v;
                can_suspend = false;
            }
        }
    });
    return can_suspend;
}


Optional<SuspendedExecutionContext> vm_execute(Value* code_buffer,
                                               int start_offset)
{
    return vm_resume({ .code_buffer_ = code_buffer,
                       .program_counter_ = start_offset});
}


// NOTE: when suspending execution, we need to place the vm stack
// somewhere. Rather than manipulating the operand stack while suspending
// execution, we create a hiden variable in the environment that we're saving.
const char* const vm_stack_save_sym = "STK";



struct VMFrame
{
    Value* code_buffer_;
    ScratchBufferPtr code_;
    Optional<ListBuilder> registers_;
    Protected saved_lexical_bindings_;
    int start_offset_;
    int nested_scope_;
    int pc_;
    u16 arguments_break_loc_;
    bool discard_flag_;
    u8 argc_;

    VMFrame(Value* code_buffer,
            ScratchBufferPtr code,
            int start_offset,
            int nested_scope,
            int pc) :
        code_buffer_(code_buffer),
        code_(code),
        saved_lexical_bindings_(L_NIL),
        start_offset_(start_offset),
        nested_scope_(nested_scope),
        pc_(pc),
        arguments_break_loc_(0),
        discard_flag_(false),
        argc_(0)
    {
    }
};



lisp::Value* vm_serialize_context(Vector<VMFrame>& ctx)
{
    ListBuilder result;

    for (auto& frame : ctx) {
        Protected contents(make_array(9));
        Array& mem = contents->array();
        mem.set(0, frame.code_buffer_);
        if (frame.registers_) {
            mem.set(1, (*frame.registers_).result());
        }
        mem.set(2, frame.saved_lexical_bindings_);
        mem.set(3, L_INT(frame.start_offset_));
        mem.set(4, L_INT(frame.nested_scope_));
        mem.set(5, L_INT(frame.pc_));
        mem.set(6, L_INT(frame.arguments_break_loc_));
        mem.set(7, make_boolean(frame.discard_flag_));
        mem.set(8, L_INT(frame.argc_));
        result.push_back(contents);
    }
    // info(stringify(result.result()));
    return result.result();
}



void vm_deserialize_context(Vector<VMFrame>& ctx, Value* saved_ctx)
{
    ctx.clear();
    // info(stringify(saved_ctx));
    l_foreach(saved_ctx, [&ctx](Value* data) {
        Array& mem = data->array();
        auto code_buffer = mem.get(0);
        auto start_offset = mem.get(3)->integer().value_;
        auto nested_scope = mem.get(4)->integer().value_;
        auto program_counter = mem.get(5)->integer().value_;

        ctx.emplace_back(code_buffer,
                         code_buffer->databuffer().value(),
                         start_offset,
                         nested_scope,
                         program_counter);

        auto& frame = ctx.back();
        auto reg_file = mem.get(1);
        if (reg_file not_eq L_NIL) {
            frame.registers_.emplace();
            while (reg_file->type() == Value::Type::cons) {
                frame.registers_->push_back(reg_file->cons().car());
                reg_file = reg_file->cons().cdr();
            }
        }
        frame.arguments_break_loc_ = mem.get(6)->integer().value_;
        frame.discard_flag_ = is_boolean_true(mem.get(7));
        frame.argc_ = mem.get(8)->integer().value_;
        frame.saved_lexical_bindings_ = mem.get(2);
    });
}



template <typename... Args> void push_error(const char* msg, Args&&... args)
{
    auto buf = allocate_small<StringBuffer<238>>({"error-msg"});
    make_format(*buf, msg, std::forward<Args>(args)...);
    push_op(make_error(buf->c_str()));
}



Optional<SuspendedExecutionContext>
vm_resume(const ExecutionContext& ctx)
{
    Value* code_buffer = ctx.code_buffer_;

    Vector<VMFrame> vm_stack(make_scratch_buffer("vm-stack-buffer"));
    vm_stack.emplace_back(code_buffer,
                          code_buffer->databuffer().value(),
                          ctx.program_counter_,
                          0,
                          ctx.program_counter_);

    VMFrame* vm_ctx = &vm_stack.back();

    // If we are within a let expression, and we want to optimize out a
    // recursive tail call, we need to unwind all frames of the lexical scope,
    // because we will never return from the optimized out function call and hit
    // the LEXICAL_FRAME_POP instruction after the tailcall instruciton.
    auto unwind_lexical_scope = [&vm_ctx] {
        while (vm_ctx->nested_scope_) {
            lexical_frame_pop();
            --vm_ctx->nested_scope_;
        }
    };

    auto check_discard_flag = [&] {
        if (vm_ctx->discard_flag_) {
            pop_op();
            vm_ctx->discard_flag_ = false;
        }
    };

    using namespace instruction;

    Protected invoke_target(L_NIL);
    int invoke_argc = 0;

#define INVOKE(FUNCTION, ARGC)        \
    invoke_target = (Value*)FUNCTION; \
    invoke_argc = ARGC;               \
    goto INVOKE_FN;                   \

    goto TOP;

INVOKE_FN:
    if (invoke_target->type() == Value::Type::function and
        invoke_target->hdr_.mode_bits_ == Function::ModeBits::lisp_bytecode_function) {

        auto obj = (Value*)invoke_target;

        if (obj->function().sig_.required_args_ > invoke_argc) {
            Protected err(make_error(Error::Code::invalid_argc, obj));
            for (int i = 0; i < invoke_argc; ++i) {
                pop_op();
            }
            push_op(err);
            check_discard_flag();
            goto TOP;
        }

        if (is_strict_mode()) {
            const auto& sig = obj->function().sig_;
            const Value::Type want[4] = {(Value::Type)sig.arg0_type_,
                                         (Value::Type)sig.arg1_type_,
                                         (Value::Type)sig.arg2_type_,
                                         (Value::Type)sig.arg3_type_};
            const int n = invoke_argc < 4 ? invoke_argc : 4;
            for (int i = 0; i < n; ++i) {
                // arg i is at operand-stack offset (invoke_argc - 1 - i)
                auto got = get_op(invoke_argc - 1 - i);
                const auto gt = got->type();
                const auto w = want[i];
                const bool ok =
                    w == Value::Type::nil or gt == w or
                    (gt == Value::Type::nil and w == Value::Type::cons) or
                    (w == Value::Type::rational and
                     (gt == Value::Type::integer or gt == Value::Type::ratio));
                if (not ok) {
                    Protected err(
                        make_error(Error::Code::invalid_argument_type, got));
                    for (int j = 0; j < invoke_argc; ++j) {
                        pop_op();
                    }
                    push_op(err);
                    check_discard_flag();
                    goto TOP;
                }
            }
        }

        vm_ctx->saved_lexical_bindings_ = lexical_bindings_ref();
        vm_ctx->arguments_break_loc_ = arguments_break_loc_ref();
        vm_ctx->argc_ = argc_ref();

        push_callstack(obj);

        gc_safepoint();

        const auto break_loc = get_op_count() - 1;
        arguments_break_loc_ref() = break_loc;
        argc_ref() = invoke_argc;

        lexical_bindings_ref() =
            dcompr(obj->function().lisp_impl_.lexical_bindings_);

        auto buf = obj->function().bytecode_impl_.databuffer();
        auto so = obj->function().bytecode_impl_.bytecode_offset()
            ->integer().value_;

        vm_stack.emplace_back(buf,
                              buf->databuffer().value(),
                              so,
                              0,
                              so);

        vm_ctx = &vm_stack.back();
        vm_ctx->argc_ = invoke_argc;

        goto TOP;

    } else {
        funcall(invoke_target, invoke_argc);
    }

    check_discard_flag();

TOP:
    while (true) {

        switch ((Opcode)vm_ctx->code_->data_[vm_ctx->pc_]) {
        case LoadReg0::op(): {
            if (not vm_ctx->registers_) {
                vm_ctx->registers_.emplace();
            }
            read<LoadReg0>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(get_list(vm_ctx->registers_->result(), 0));
            break;
        }

        case LoadReg1::op(): {
            if (not vm_ctx->registers_) {
                vm_ctx->registers_.emplace();
            }
            read<LoadReg1>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(get_list(vm_ctx->registers_->result(), 1));
            break;
        }

        case LoadReg2::op(): {
            if (not vm_ctx->registers_) {
                vm_ctx->registers_.emplace();
            }
            read<LoadReg2>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(get_list(vm_ctx->registers_->result(), 2));
            break;
        }

        case LoadReg::op(): {
            if (not vm_ctx->registers_) {
                vm_ctx->registers_.emplace();
            }
            auto inst = read<LoadReg>(*vm_ctx->code_, vm_ctx->pc_);
            auto v = get_list(vm_ctx->registers_->result(), inst->reg_);
            // info(::format("load % from reg %", v, inst->reg_));
            push_op(v);
            break;
        }

        case StoreReg0::op(): {
            if (not vm_ctx->registers_) {
                vm_ctx->registers_.emplace();
            }
            read<StoreReg0>(*vm_ctx->code_, vm_ctx->pc_);
            while (vm_ctx->registers_->length() <= 0) {
                vm_ctx->registers_->push_back(L_NIL);
            }
            auto iter = vm_ctx->registers_->result();
            iter->cons().set_car(get_op0());
            pop_op();
            break;
        }

        case StoreReg1::op(): {
            if (not vm_ctx->registers_) {
                vm_ctx->registers_.emplace();
            }
            read<StoreReg1>(*vm_ctx->code_, vm_ctx->pc_);
            while (vm_ctx->registers_->length() <= 1) {
                vm_ctx->registers_->push_back(L_NIL);
            }
            u8 reg = 1;
            auto iter = vm_ctx->registers_->result();
            while (reg) {
                --reg;
                iter = iter->cons().cdr();
            }
            iter->cons().set_car(get_op0());
            pop_op();
            break;
        }

        case StoreReg2::op(): {
            if (not vm_ctx->registers_) {
                vm_ctx->registers_.emplace();
            }
            read<StoreReg2>(*vm_ctx->code_, vm_ctx->pc_);
            while (vm_ctx->registers_->length() <= 2) {
                vm_ctx->registers_->push_back(L_NIL);
            }
            u8 reg = 2;
            auto iter = vm_ctx->registers_->result();
            while (reg) {
                --reg;
                iter = iter->cons().cdr();
            }
            iter->cons().set_car(get_op0());
            pop_op();
            break;
        }

        case StoreReg::op(): {
            if (not vm_ctx->registers_) {
                vm_ctx->registers_.emplace();
            }
            auto inst = read<StoreReg>(*vm_ctx->code_, vm_ctx->pc_);
            while (vm_ctx->registers_->length() <= inst->reg_) {
                vm_ctx->registers_->push_back(L_NIL);
            }
            u8 reg = inst->reg_;
            auto iter = vm_ctx->registers_->result();
            while (reg) {
                --reg;
                iter = iter->cons().cdr();
            }
            // info(::format("store % into reg %", get_op0(), inst->reg_));
            iter->cons().set_car(get_op0());
            pop_op();
            break;
        }

        case StoreRegKeep::op(): {
            if (not vm_ctx->registers_) {
                vm_ctx->registers_.emplace();
            }
            auto inst = read<StoreReg>(*vm_ctx->code_, vm_ctx->pc_);
            while (vm_ctx->registers_->length() <= inst->reg_) {
                vm_ctx->registers_->push_back(L_NIL);
            }
            u8 reg = inst->reg_;
            auto iter = vm_ctx->registers_->result();
            while (reg) {
                --reg;
                iter = iter->cons().cdr();
            }
            iter->cons().set_car(get_op0());
            break;
        }

        case StoreReg0Keep::op(): {
            if (not vm_ctx->registers_) {
                vm_ctx->registers_.emplace();
            }
            read<StoreReg0Keep>(*vm_ctx->code_, vm_ctx->pc_);
            while (vm_ctx->registers_->length() <= 0) {
                vm_ctx->registers_->push_back(L_NIL);
            }
            u8 reg = 0;
            auto iter = vm_ctx->registers_->result();
            while (reg) {
                --reg;
                iter = iter->cons().cdr();
            }
            iter->cons().set_car(get_op0());
            break;
        }

        case StoreReg1Keep::op(): {
            if (not vm_ctx->registers_) {
                vm_ctx->registers_.emplace();
            }
            read<StoreReg1Keep>(*vm_ctx->code_, vm_ctx->pc_);
            while (vm_ctx->registers_->length() <= 1) {
                vm_ctx->registers_->push_back(L_NIL);
            }
            u8 reg = 1;
            auto iter = vm_ctx->registers_->result();
            while (reg) {
                --reg;
                iter = iter->cons().cdr();
            }
            iter->cons().set_car(get_op0());
            break;
        }

        case StoreReg2Keep::op(): {
            if (not vm_ctx->registers_) {
                vm_ctx->registers_.emplace();
            }
            read<StoreReg2Keep>(*vm_ctx->code_, vm_ctx->pc_);
            while (vm_ctx->registers_->length() <= 2) {
                vm_ctx->registers_->push_back(L_NIL);
            }
            u8 reg = 2;
            auto iter = vm_ctx->registers_->result();
            while (reg) {
                --reg;
                iter = iter->cons().cdr();
            }
            iter->cons().set_car(get_op0());
            break;
        }

        case JumpIfFalse::op(): {
            auto inst = read<JumpIfFalse>(*vm_ctx->code_, vm_ctx->pc_);
            if (not is_boolean_true(get_op0())) {
                vm_ctx->pc_ = vm_ctx->start_offset_ + inst->offset_.get();
            }
            pop_op();
            break;
        }

        case Jump::op(): {
            auto inst = read<Jump>(*vm_ctx->code_, vm_ctx->pc_);
            vm_ctx->pc_ = vm_ctx->start_offset_ + inst->offset_.get();
            break;
        }

        case SmallJumpIfFalse::op(): {
            auto inst = read<SmallJumpIfFalse>(*vm_ctx->code_, vm_ctx->pc_);
            if (not is_boolean_true(get_op0())) {
                vm_ctx->pc_ = vm_ctx->start_offset_ + inst->offset_;
            }
            pop_op();
            break;
        }

        case SmallJumpIfTrue::op(): {
            auto inst = read<SmallJumpIfTrue>(*vm_ctx->code_, vm_ctx->pc_);
            if (is_boolean_true(get_op0())) {
                vm_ctx->pc_ = vm_ctx->start_offset_ + inst->offset_;
            }
            pop_op();
            break;
        }

        case SmallJumpNotEqual::op(): {
            auto inst = read<SmallJumpNotEqual>(*vm_ctx->code_, vm_ctx->pc_);
            auto result = builtin_comp_equal(2);
            pop_op();
            pop_op(); // args
            if (not is_boolean_true(result)) {
                vm_ctx->pc_ = vm_ctx->start_offset_ + inst->offset_;
            }
            break;
        }

        case SmallJump::op(): {
            auto inst = read<SmallJump>(*vm_ctx->code_, vm_ctx->pc_);
            vm_ctx->pc_ = vm_ctx->start_offset_ + inst->offset_;
            break;
        }


        case Set::op(): {
            read<Set>(*vm_ctx->code_, vm_ctx->pc_);
            auto sym = get_op1();
            auto v = get_op0();
            pop_op();
            pop_op();
            if (sym->type() not_eq Value::Type::symbol) {
                push_op(make_error(
                    ::format<48>("non-symbol % passed to set!", sym).c_str()));
            } else {
                push_op(set_var(sym, v, false));
            }
            break;
        }


        case LoadSymtab::op(): {
            auto inst = read<LoadSymtab>(*vm_ctx->code_, vm_ctx->pc_);
            auto index = inst->symtab_index_.get();
            push_op(make_symtab_symbol(index));
            break;
        }


        case Resume::op(): {
            if (auto stk_loc = __find_local(vm_stack_save_sym)) {
                if (auto data = __get_local(*stk_loc)) {
                    vm_deserialize_context(vm_stack, data);
                    vm_ctx = &vm_stack.back();
                 }
            } else {
                PLATFORM.fatal("resume failed: missing STK");
            }
            read<Resume>(*vm_ctx->code_, vm_ctx->pc_);
            lexical_frame_pop();
            break;
        }


        case Await::op(): {
            read<Await>(*vm_ctx->code_, vm_ctx->pc_);
            if (get_op0()->type() not_eq lisp::Value::Type::promise) {
                pop_op();
                push_op(make_error("await expects a promise value!"));
            } else {
                Value* agitant = L_NIL;
                if (vm_can_suspend(agitant)) {
                    lexical_frame_push();
                    lexical_frame_store(L_CONS(make_symbol(vm_stack_save_sym),
                                               vm_serialize_context(vm_stack)));
                    SuspendedExecutionContext suspend{
                        .code_buffer_ = vm_ctx->code_buffer_,
                        .program_counter_ = vm_ctx->pc_};
                    return suspend;
                } else {
                    pop_op(); // the promise value
                    auto err_str = ::format("await failed: compiled caller % "
                                            "cannot call functions that await",
                                            agitant);
                    push_op(make_error(err_str.c_str()));
                    // NOTE: this is required, because otherwise, we will
                    // execute the subsequent instruction, the resume
                    // instruction, but there isn't actually a valid suspended
                    // state to resume from!
                    goto RETURN;
                }
            }
            break;
        }

        case SetVar::op(): {
            auto inst = read<SetVar>(*vm_ctx->code_, vm_ctx->pc_);
            auto index = inst->symtab_index_.get();
            auto v = get_op0();
            Protected sym(make_symtab_symbol(index));
            auto result = set_var(sym, v, false);
            pop_op(); // v
            push_op(result);
            break;
        }

        case ConsVar::op(): {
            auto inst = read<SetVar>(*vm_ctx->code_, vm_ctx->pc_);
            auto index = inst->symtab_index_.get();
            auto v = get_op0();
            Protected sym(make_symtab_symbol(index));
            Protected lat = get_var(sym);
            lat = make_cons(v, lat);
            auto result = set_var(sym, lat, false);
            pop_op(); // v
            push_op(result);
            break;
        }

        case SetVarRT::op(): {
            auto inst = read<SetVarRT>(*vm_ctx->code_, vm_ctx->pc_);
            auto v = get_op0();
            Protected sym(make_symbol(inst->ptr_.get(),
                                      Symbol::ModeBits::stable_pointer));
            auto result = set_var(sym, v, false);
            pop_op(); // v
            push_op(result);
            break;
        }

        case SetVarSmall::op(): {
            auto inst = read<SetVarSmall>(*vm_ctx->code_, vm_ctx->pc_);
            StringBuffer<3> name;
            for (int i = 0; i < 3; ++i) {
                name.__push_unsafe(inst->name_[i]);
            }
            Protected sym(
                make_symbol(name.c_str(), Symbol::ModeBits::stable_pointer));
            auto v = get_op0();
            auto result = set_var(sym, v, false);
            pop_op(); // v
            push_op(result);
            break;
        }

        case LoadVarS::op(): {
            auto inst = read<LoadVarS>(*vm_ctx->code_, vm_ctx->pc_);
            auto index = inst->symtab_index_.get();
            auto sym = make_symtab_symbol(index);
            push_op(get_var(sym));
            collect_value(sym);
            break;
        }

        case LoadVarRT::op(): {
            auto inst = read<LoadVarRT>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(get_var_stable(inst->ptr_.get()));
            break;
        }

        case LoadVarSmall::op(): {
            auto inst = read<LoadVarSmall>(*vm_ctx->code_, vm_ctx->pc_);
            StringBuffer<3> name;
            for (int i = 0; i < 3; ++i) {
                name.__push_unsafe(inst->name_[i]);
            }
            push_op(get_var_stable(name.c_str()));
            break;
        }

        case Dup::op(): {
            read<Dup>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(get_op0());
            break;
        }

        case Not::op(): {
            read<Not>(*vm_ctx->code_, vm_ctx->pc_);
            auto input = get_op0();
            pop_op();
            push_op(make_integer(not is_boolean_true(input)));
            break;
        }

        case PushNil::op():
            read<PushNil>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(get_nil());
            break;

        case PushFloat::op(): {
            auto inst = read<PushFloat>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(make_float(inst->f_.get()));
            break;
        }

        case PushRatio::op(): {
            auto inst = read<PushRatio>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(make_ratio(inst->num_.get(), inst->div_.get()));
            break;
        }

        case PushInteger::op(): {
            auto inst = read<PushInteger>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(make_integer(inst->value_.get()));
            break;
        }

        case Push0::op():
            read<Push0>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(make_integer(0));
            break;

        case Push1::op():
            read<Push1>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(make_integer(1));
            break;

        case Push2::op():
            read<Push2>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(make_integer(2));
            break;

        case PushSmallInteger::op(): {
            auto inst = read<PushSmallInteger>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(make_integer(inst->value_));
            break;
        }

        case PushSmallSymbol::op(): {
            auto inst = read<PushSmallSymbol>(*vm_ctx->code_, vm_ctx->pc_);
            StringBuffer<3> str;
            auto name = inst->name_;
            for (int i = 0; i < 3; ++i) {
                str.__push_unsafe(name[i]);
            }
            push_op(make_symbol(str.c_str(), Symbol::ModeBits::small));
            break;
        }

        case PushSymbolRT::op(): {
            auto inst = read<PushSymbolRT>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(make_symbol(inst->ptr_.get(),
                                Symbol::ModeBits::stable_pointer));
            break;
        }

        case PushString::op(): {
            auto inst = read<PushString>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(make_string(vm_ctx->code_->data_ + vm_ctx->pc_));
            vm_ctx->pc_ += inst->length_;
            break;
        }

        case TailCall::op(): {

            Protected fn(get_op0());

            auto argc = read<TailCall>(*vm_ctx->code_, vm_ctx->pc_)->argc_;


            if (fn == get_this()) {
                pop_op(); // function on stack

                if (get_argc() not_eq argc) {
                    // TODO: raise error: attempted recursive call with
                    // different number of args than current function.
                    // Actually...
                    // The isn't really anything preventing a variadic function
                    // from being executed recursively with a different number
                    // of args, right? So maybe shouldn't be isn't an error...
                    while (true)
                        ;
                }

                if (argc == 0) {
                    unwind_lexical_scope();
                    vm_ctx->pc_ = vm_ctx->start_offset_;
                    vm_ctx->registers_.reset();
                    goto TOP;
                } else {
                    // TODO: perform TCO for N-arg function
                    INVOKE(fn, argc);
                }

            } else {

                pop_op();
                INVOKE(fn, argc);
            }

            break;
        }

        case LoadTCall1::op(): {
            auto inst = read<LoadTCall1>(*vm_ctx->code_, vm_ctx->pc_);
            auto index = inst->symtab_index_.get();
            Protected sym(make_symtab_symbol(index));
            Protected fn(get_var(sym));

            if (fn == get_this()) {
                auto arg = get_op0();

                if (get_argc() not_eq 1) {
                    // TODO: raise error: attempted recursive call with
                    // different number of args than current function.
                    while (true)
                        ;
                }

                pop_op(); // argument
                pop_op(); // previous arg

                push_op(arg);

                unwind_lexical_scope();
                vm_ctx->pc_ = vm_ctx->start_offset_;
                vm_ctx->registers_.reset();
                goto TOP;

            } else {
                INVOKE(fn, 1);
            }
            break;
        }

        case LoadTCall2::op(): {
            auto inst = read<LoadTCall2>(*vm_ctx->code_, vm_ctx->pc_);
            auto index = inst->symtab_index_.get();
            Protected sym(make_symtab_symbol(index));
            Protected fn(get_var(sym));

            if (fn == get_this()) {
                auto arg0 = get_op0();
                auto arg1 = get_op1();

                if (get_argc() not_eq 2) {
                    // TODO: raise error: attempted recursive call with
                    // different number of args than current function.
                    while (true)
                        ;
                }

                pop_op(); // arg
                pop_op(); // arg
                pop_op(); // prev arg
                pop_op(); // prev arg

                push_op(arg1);
                push_op(arg0);

                unwind_lexical_scope();
                vm_ctx->pc_ = vm_ctx->start_offset_;
                vm_ctx->registers_.reset();
                goto TOP;

            } else {
                INVOKE(fn, 2);
            }
            break;
        }

        case LoadTCall3::op(): {
            auto inst = read<LoadTCall3>(*vm_ctx->code_, vm_ctx->pc_);
            auto index = inst->symtab_index_.get();
            Protected sym(make_symtab_symbol(index));
            Protected fn(get_var(sym));

            if (fn == get_this()) {
                auto arg0 = get_op0();
                auto arg1 = get_op1();
                auto arg2 = get_op(2);

                if (get_argc() not_eq 3) {
                    while (true)
                        ;
                }

                pop_op(); // arg
                pop_op(); // arg
                pop_op(); // arg
                pop_op(); // prev arg
                pop_op(); // prev arg
                pop_op(); // prev arg

                push_op(arg2);
                push_op(arg1);
                push_op(arg0);

                unwind_lexical_scope();
                vm_ctx->pc_ = vm_ctx->start_offset_;
                vm_ctx->registers_.reset();
                goto TOP;

            } else {
                INVOKE(fn, 3);
            }
            break;
        }

        case TailCall1::op(): {
            read<TailCall1>(*vm_ctx->code_, vm_ctx->pc_);
            Protected fn(get_op0());

            if (fn == get_this()) {
                auto arg = get_op1();

                if (get_argc() not_eq 1) {
                    // TODO: raise error: attempted recursive call with
                    // different number of args than current function.
                    while (true)
                        ;
                }

                pop_op(); // function on stack
                pop_op(); // argument
                pop_op(); // previous arg

                push_op(arg);

                unwind_lexical_scope();
                vm_ctx->pc_ = vm_ctx->start_offset_;
                vm_ctx->registers_.reset();
                goto TOP;

            } else {
                pop_op();
                INVOKE(fn, 1);
            }
            break;
        }

        case TailCall2::op(): {
            read<TailCall2>(*vm_ctx->code_, vm_ctx->pc_);
            Protected fn(get_op0());

            if (fn == get_this()) {
                auto arg0 = get_op1();
                auto arg1 = get_op(2);

                if (get_argc() not_eq 2) {
                    // TODO: raise error: attempted recursive call with
                    // different number of args than current function.
                    while (true)
                        ;
                }

                pop_op(); // function on stack
                pop_op(); // arg
                pop_op(); // arg
                pop_op(); // prev arg
                pop_op(); // prev arg

                push_op(arg1);
                push_op(arg0);

                unwind_lexical_scope();
                vm_ctx->pc_ = vm_ctx->start_offset_;
                vm_ctx->registers_.reset();
                goto TOP;

            } else {
                pop_op();
                INVOKE(fn, 2);
            }
            break;
        }

        case TailCall3::op(): {
            read<TailCall3>(*vm_ctx->code_, vm_ctx->pc_);
            Protected fn(get_op0());

            if (fn == get_this()) {
                auto arg0 = get_op1();
                auto arg1 = get_op(2);
                auto arg2 = get_op(3);

                if (get_argc() not_eq 3) {
                    while (true)
                        ;
                }

                pop_op(); // function on stack
                pop_op(); // arg
                pop_op(); // arg
                pop_op(); // arg
                pop_op(); // prev arg
                pop_op(); // prev arg
                pop_op(); // prev arg

                push_op(arg2);
                push_op(arg1);
                push_op(arg0);

                unwind_lexical_scope();
                vm_ctx->pc_ = vm_ctx->start_offset_;
                vm_ctx->registers_.reset();
                goto TOP;

            } else {
                pop_op();
                INVOKE(fn, 3);
            }
            break;
        }

        case LoadCall0::op(): {
            auto inst = read<LoadCall0>(*vm_ctx->code_, vm_ctx->pc_);
            auto index = inst->symtab_index_.get();
            Protected sym(make_symtab_symbol(index));
            auto fn = get_var(sym);
            collect_value(sym);
            INVOKE(fn, 0);
            break;
        }

        case LoadCall1::op(): {
            auto inst = read<LoadCall1>(*vm_ctx->code_, vm_ctx->pc_);
            auto index = inst->symtab_index_.get();
            Protected sym(make_symtab_symbol(index));
            auto fn = get_var(sym);
            collect_value(sym);
            INVOKE(fn, 1);
            break;
        }

        case LoadCall2::op(): {
            auto inst = read<LoadCall2>(*vm_ctx->code_, vm_ctx->pc_);
            auto index = inst->symtab_index_.get();
            Protected sym(make_symtab_symbol(index));
            auto fn = get_var(sym);
            collect_value(sym);
            INVOKE(fn, 2);
            break;
        }

        case LoadCall3::op(): {
            auto inst = read<LoadCall3>(*vm_ctx->code_, vm_ctx->pc_);
            auto index = inst->symtab_index_.get();
            Protected sym(make_symtab_symbol(index));
            auto fn = get_var(sym);
            collect_value(sym);
            INVOKE(fn, 3);
            break;
        }

        case LoadCall0Discard::op(): {
            auto inst = read<LoadCall0Discard>(*vm_ctx->code_, vm_ctx->pc_);
            auto index = inst->symtab_index_.get();
            Protected sym(make_symtab_symbol(index));
            Protected fn(get_var(sym));
            collect_value(sym);
            vm_ctx->discard_flag_ = true;
            INVOKE(fn, 0);
            break;
        }

        case LoadCall1Discard::op(): {
            auto inst = read<LoadCall1Discard>(*vm_ctx->code_, vm_ctx->pc_);
            auto index = inst->symtab_index_.get();
            Protected sym(make_symtab_symbol(index));
            Protected fn(get_var(sym));
            collect_value(sym);
            vm_ctx->discard_flag_ = true;
            INVOKE(fn, 1);
            break;
        }

        case LoadCall2Discard::op(): {
            auto inst = read<LoadCall2Discard>(*vm_ctx->code_, vm_ctx->pc_);
            auto index = inst->symtab_index_.get();
            Protected sym(make_symtab_symbol(index));
            Protected fn(get_var(sym));
            collect_value(sym);
            vm_ctx->discard_flag_ = true;
            INVOKE(fn, 2);
            break;
        }

        case LoadCall3Discard::op(): {
            auto inst = read<LoadCall3Discard>(*vm_ctx->code_, vm_ctx->pc_);
            auto index = inst->symtab_index_.get();
            Protected sym(make_symtab_symbol(index));
            Protected fn(get_var(sym));
            collect_value(sym);
            vm_ctx->discard_flag_ = true;
            INVOKE(fn, 3);
            break;
        }

        case Funcall::op(): {
            auto fn = get_op0();
            auto argc = read<Funcall>(*vm_ctx->code_, vm_ctx->pc_)->argc_;
            pop_op();
            INVOKE(fn, argc);
            break;
        }

        case Funcall1::op(): {
            read<Funcall1>(*vm_ctx->code_, vm_ctx->pc_);
            auto fn = get_op0();
            pop_op();
            INVOKE(fn, 1);
            break;
        }

        case Funcall2::op(): {
            read<Funcall2>(*vm_ctx->code_, vm_ctx->pc_);
            auto fn = get_op0();
            pop_op();
            INVOKE(fn, 2);
            break;
        }

        case Funcall3::op(): {
            read<Funcall3>(*vm_ctx->code_, vm_ctx->pc_);
            auto fn = get_op0();
            pop_op();
            INVOKE(fn, 3);
            break;
        }

        case Arg::op(): {
            read<Arg>(*vm_ctx->code_, vm_ctx->pc_);
            auto arg_num = get_op0();
            auto arg = get_arg(arg_num->integer().value_);
            pop_op();
            push_op(arg);
            break;
        }

        case Arg0::op(): {
            read<Arg0>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(get_arg(0));
            break;
        }

        case Arg1::op(): {
            read<Arg1>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(get_arg(1));
            break;
        }

        case Arg2::op(): {
            read<Arg2>(*vm_ctx->code_, vm_ctx->pc_);
            push_op(get_arg(2));
            break;
        }

        case MakePair::op(): {
            read<MakePair>(*vm_ctx->code_, vm_ctx->pc_);
            auto car = get_op1();
            auto cdr = get_op0();
            auto cons = make_cons(car, cdr);
            pop_op();
            pop_op();
            push_op(cons);
            break;
        }

        case First::op(): {
            read<First>(*vm_ctx->code_, vm_ctx->pc_);
            auto arg = get_op0();
            pop_op();
            if (arg->type() == Value::Type::cons) {
                push_op(arg->cons().car());
            } else {
                push_op(make_error(Error::Code::invalid_argument_type, L_NIL));
            }
            break;
        }

        case Rest::op(): {
            read<Rest>(*vm_ctx->code_, vm_ctx->pc_);
            auto arg = get_op0();
            pop_op();
            if (arg->type() == Value::Type::cons) {
                push_op(arg->cons().cdr());
            } else {
                push_op(make_error(Error::Code::invalid_argument_type, L_NIL));
            }
            break;
        }

        case Pop::op():
            read<Pop>(*vm_ctx->code_, vm_ctx->pc_);
            pop_op();
            break;

        case RetNilIfFalseKeep::op(): {
            read<RetNilIfFalseKeep>(*vm_ctx->code_, vm_ctx->pc_);
            auto cond = get_op0();
            if (is_boolean_true(cond)) {
                break;
            }
            pop_op();
            goto RETURN_NIL;
        }

        case RetNilIfTrue::op(): {
            read<RetNilIfTrue>(*vm_ctx->code_, vm_ctx->pc_);
            auto cond = get_op0();
            pop_op();
            if (not is_boolean_true(cond)) {
                break;
            }
            goto RETURN_NIL;
        }

        case RetNilIfFalse::op(): {
            read<RetNilIfFalse>(*vm_ctx->code_, vm_ctx->pc_);
            auto cond = get_op0();
            pop_op();
            if (is_boolean_true(cond)) {
                break;
            }
            goto RETURN_NIL;
        }

        case EarlyRetNil::op():
        case RetNil::op():
        RETURN_NIL:
            push_op(L_NIL);
            goto RETURN;

        case EarlyRet::op():
        case Ret::op():
        RETURN:
            if (vm_ctx->registers_) {
                Value* lat = vm_ctx->registers_->result();
                while (lat->type() == Value::Type::cons) {
                    auto next = lat->cons().cdr();
                    collect_value(lat);
                    lat = next;
                }
            }
            unwind_lexical_scope();
            if (vm_stack.size() > 1) {
                auto result = get_op0();
                pop_op();
                for (int i = 0; i < vm_ctx->argc_; ++i) {
                    pop_op();
                }
                push_op(result);
                vm_stack.pop_back();
                vm_ctx = &vm_stack.back();
                pop_callstack();
                lexical_bindings_ref() = vm_ctx->saved_lexical_bindings_;
                argc_ref() = vm_ctx->argc_;
                arguments_break_loc_ref() = vm_ctx->arguments_break_loc_;
                if (vm_ctx->discard_flag_) {
                    pop_op();
                    vm_ctx->discard_flag_ = false;
                }
                goto TOP;
            }
            return nullopt();

        case PushLambda::op(): {
            auto inst = read<PushLambda>(*vm_ctx->code_, vm_ctx->pc_);
            auto offset = make_integer(vm_ctx->pc_);
            if (offset->type() == lisp::Value::Type::integer) {
                auto bytecode = make_cons(offset, vm_ctx->code_buffer_);
                if (bytecode->type() == lisp::Value::Type::cons) {
                    auto fn = make_bytecode_function(bytecode);
                    push_op(fn);
                } else {
                    push_op(bytecode);
                }
            } else {
                push_op(offset);
            }
            vm_ctx->pc_ = vm_ctx->start_offset_ + inst->lambda_end_.get();
            break;
        }

        case PushList::op(): {
            auto list_size = read<PushList>(*vm_ctx->code_, vm_ctx->pc_)->element_count_;
            ListBuilder lat;
            for (int i = 0; i < list_size; ++i) {
                lat.push_back(get_op((list_size - 1) - i));
            }
            for (int i = 0; i < list_size; ++i) {
                pop_op();
            }
            push_op(lat.result());
            break;
        }

        case PushThis::op(): {
            push_op(get_this());
            read<PushThis>(*vm_ctx->code_, vm_ctx->pc_);
            break;
        }

        case LexicalDef::op(): {
            auto inst = read<LexicalDef>(*vm_ctx->code_, vm_ctx->pc_);
            auto index = inst->symtab_index_.get();
            Protected sym(make_symtab_symbol(index));

            // pair of (sym . value)
            auto pair = make_cons(sym, get_op0());
            pop_op();      // pop value
            push_op(pair); // store pair

            lexical_frame_store(pair);
            pop_op();
            break;
        }

        case LexicalDefRT::op(): {
            auto inst = read<LexicalDefRT>(*vm_ctx->code_, vm_ctx->pc_);
            Protected sym(make_symbol(inst->ptr_.get(),
                                      Symbol::ModeBits::stable_pointer));

            // pair of (sym . value)
            auto pair = make_cons(sym, get_op0());
            pop_op();      // pop value
            push_op(pair); // store pair

            lexical_frame_store(pair);
            pop_op();
            break;
        }

        case LexicalDefSmall::op(): {
            auto inst = read<LexicalDefSmall>(*vm_ctx->code_, vm_ctx->pc_);

            StringBuffer<3> name;
            for (int i = 0; i < 3; ++i) {
                name.__push_unsafe(inst->name_[i]);
            }

            Protected sym(make_symbol(name.c_str(), Symbol::ModeBits::small));

            // pair of (sym . value)
            auto pair = make_cons(sym, get_op0());
            pop_op();      // pop value
            push_op(pair); // store pair

            lexical_frame_store(pair);
            pop_op();
            break;
        }

        case LexicalFramePush::op(): {
            read<LexicalFramePush>(*vm_ctx->code_, vm_ctx->pc_);
            lexical_frame_push();
            ++vm_ctx->nested_scope_;
            break;
        }

        case LexicalFramePop::op(): {
            read<LexicalFramePop>(*vm_ctx->code_, vm_ctx->pc_);
            lexical_frame_pop();
            --vm_ctx->nested_scope_;
            break;
        }

        case BitAnd::op(): {
            read<BitAnd>(*vm_ctx->code_, vm_ctx->pc_);
            auto result = builtin_bit_and(2);
            pop_op();
            pop_op();
            push_op(result);
            break;
        }

        case BitOr::op(): {
            read<BitOr>(*vm_ctx->code_, vm_ctx->pc_);
            auto result = builtin_bit_or(2);
            pop_op();
            pop_op();
            push_op(result);
            break;
        }

        case BitNot::op(): {
            read<BitNot>(*vm_ctx->code_, vm_ctx->pc_);
            auto result = builtin_bit_not(1);
            pop_op();
            push_op(result);
            break;
        }

        case BitShiftLeft::op(): {
            read<BitShiftLeft>(*vm_ctx->code_, vm_ctx->pc_);
            auto result = builtin_bit_shift_left(2);
            pop_op();
            pop_op();
            push_op(result);
            break;
        }

        case DestructureAssertPair::op(): {
            read<DestructureAssertPair>(*vm_ctx->code_, vm_ctx->pc_);
            auto top = get_op0();
            pop_op();
            push_op(make_boolean(top->type() == Value::Type::cons and not is_list(top)));
            break;
        }

        case DestructureAssertList::op(): {
            auto inst = read<DestructureAssertList>(*vm_ctx->code_, vm_ctx->pc_);
            auto top = get_op0();
            auto len = length(top);
            pop_op();
            push_op(make_boolean(is_list(top) and len == inst->len_));
            break;
        }

        case BitShiftRight::op(): {
            read<BitShiftRight>(*vm_ctx->code_, vm_ctx->pc_);
            auto result = builtin_bit_shift_right(2);
            pop_op();
            pop_op();
            push_op(result);
            break;
        }

        case Add::op(): {
            auto add = read<Add>(*vm_ctx->code_, vm_ctx->pc_);
            auto result = builtin_add(add->operands_);
            for (int i = 0; i < add->operands_; ++i) {
                pop_op();
            }
            push_op(result);
            break;
        }

        case Subtract::op(): {
            read<Subtract>(*vm_ctx->code_, vm_ctx->pc_);
            auto result = builtin_subtract(2);
            pop_op();
            pop_op();
            push_op(result);
            break;
        }

        case Divide::op(): {
            read<Divide>(*vm_ctx->code_, vm_ctx->pc_);
            auto result = builtin_divide(2);
            pop_op();
            pop_op();
            push_op(result);
            break;
        }

        case Multiply::op(): {
            auto multiply = read<Multiply>(*vm_ctx->code_, vm_ctx->pc_);
            auto result = builtin_multiply(multiply->operands_);
            for (int i = 0; i < multiply->operands_; ++i) {
                pop_op();
            }
            push_op(result);
            break;
        }

        case Incr::op(): {
            read<Incr>(*vm_ctx->code_, vm_ctx->pc_);
            auto result = builtin_incr(1);
            pop_op();
            push_op(result);
            break;
        }

        case Decr::op(): {
            read<Decr>(*vm_ctx->code_, vm_ctx->pc_);
            auto result = builtin_decr(1);
            pop_op();
            push_op(result);
            break;
        }

        case Length::op(): {
            read<Length>(*vm_ctx->code_, vm_ctx->pc_);
            auto result = builtin_length(1);
            pop_op();
            push_op(result);
            break;
        }

        case Get::op(): {
            read<Get>(*vm_ctx->code_, vm_ctx->pc_);
            auto result = builtin_get(2);
            pop_op();
            pop_op();
            push_op(result);
            break;
        }

        case IsEqual::op(): {
            read<IsEqual>(*vm_ctx->code_, vm_ctx->pc_);
            auto result = builtin_comp_equal(2);
            pop_op();
            pop_op(); // args
            push_op(result);
            break;
        }

        case CmpLess::op(): {
            read<CmpLess>(*vm_ctx->code_, vm_ctx->pc_);
            auto result = builtin_comp_less_than(2);
            pop_op();
            pop_op(); // args
            push_op(result);
            break;
        }

        case CmpGreater::op(): {
            read<CmpGreater>(*vm_ctx->code_, vm_ctx->pc_);
            auto result = builtin_comp_greater_than(2);
            pop_op();
            pop_op(); // args
            push_op(result);
            break;
        }

        default:
        case Fatal::op():
            while (true)
                ;
            break;
        }
    }

    return nullopt();
}


} // namespace lisp
