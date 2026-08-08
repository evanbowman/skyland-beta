////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "value.hpp"


namespace lisp
{


// Protected objects will not be collected until the Protected wrapper goes out
// of scope.

class Protected final
{
private:
    static Protected* __protected_values;

public:
    Protected(Value* val) : val_(val)
    {
        prev_ = nullptr;
        next_ = __protected_values;

        if (__protected_values) {
            __protected_values->prev_ = this;
        }

        __protected_values = this;
    }


    Protected(const Protected&) = delete;


    Protected(Protected&&) = delete;


    ~Protected()
    {
        if (prev_ == nullptr) {
            // We're the list head!
            __protected_values = next_;
        } else {
            prev_->next_ = next_;
        }

        if (next_) {
            next_->prev_ = prev_;
        }
    }


    static void mark_all();
    void gc_mark();


    Protected* next() const
    {
        return next_;
    }

    Protected* prev() const
    {
        return prev_;
    }


    Protected& operator=(Value* val)
    {
        val_ = val;
        return *this;
    }

    void set(Value* val)
    {
        val_ = val;
    }

    operator Value*()
    {
        return val_;
    }

    Value* get() const
    {
        return val_;
    }

    Value* operator->()
    {
        return val_;
    }

private:
    Value* val_;
    Protected* prev_;
    Protected* next_;
};


} // namespace lisp
