////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#pragma once

#include "optional.hpp"
#include "value.hpp"



namespace lisp
{



struct ExecutionContext
{
    Value* code_buffer_;
    int program_counter_;
};


using SuspendedExecutionContext = ExecutionContext;


Optional<SuspendedExecutionContext>
vm_resume(const ExecutionContext& ctx);



Optional<SuspendedExecutionContext> vm_execute(Value* code_buffer,
                                               int start_offset);



} // namespace lisp
