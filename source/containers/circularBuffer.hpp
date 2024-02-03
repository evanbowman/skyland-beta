////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024  Evan Bowman. Some rights reserved.
//
// This program is source-available; the source code is provided for educational
// purposes. All copies of the software must be distributed along with this
// license document.
//
// 1. DEFINITION OF SOFTWARE: The term "Software" refers to SKYLAND,
// including any updates, modifications, or associated documentation provided by
// Licensor.
//
// 2. DERIVATIVE WORKS: Licensee is permitted to modify the source code.
//
// 3. COMMERCIAL USE: Commercial use is not allowed.
//
// 4. ATTRIBUTION: Licensee is required to provide attribution to Licensor.
//
// 5. INTELLECTUAL PROPERTY RIGHTS: All intellectual property rights in the
// Software shall remain the property of Licensor. The Licensee does not acquire
// any rights to the Software except for the limited use rights specified in
// this Agreement.
//
// 6. WARRANTY AND LIABILITY: The Software is provided "as is" without warranty
// of any kind. Licensor shall not be liable for any damages arising out of or
// related to the use or inability to use the Software.
//
// 7. TERMINATION: This Agreement shall terminate automatically if Licensee
// breaches any of its terms and conditions. Upon termination, Licensee must
// cease all use of the Software and destroy all copies.
//
////////////////////////////////////////////////////////////////////////////////

#pragma once

#include "../number/int.h"
#include <new>
#include "../optional.hpp"



template <typename T, int capacity>
class CircularBuffer
{
public:

    static_assert(capacity % 2 == 0);


    CircularBuffer() : head_(0), tail_(0), full_(false)
    {
    }


    CircularBuffer(const CircularBuffer&) = delete;


    ~CircularBuffer()
    {
        while (pop()) ;
    }


    bool full() const
    {
        return full_;
    }


    bool empty() const
    {
        return not full_ and head_ == tail_;
    }


    void push(T&& val)
    {
        if (full()) {
            tail_ = (tail_ + 1) % capacity;
            ((T*)memory_[head_].mem_)->~T();
        }
        new ((T*)memory_[head_].mem_) T(std::forward<T>(val));
        head_ = (head_ + 1) % capacity;
        full_ = head_ == tail_;
    }


    void push(const T& val)
    {
        if (full()) {
            tail_ = (tail_ + 1) % capacity;
            ((T*)memory_[head_].mem_)->~T();
        }
        new ((T*)memory_[head_].mem_) T(val);
        head_ = (head_ + 1) % capacity;
        full_ = head_ == tail_;
    }


    Optional<T> pop()
    {
        if (empty()) {
            return nullopt();
        }

        auto val = std::move(*((T*)memory_[tail_].mem_));
        ((T*)memory_[tail_].mem_)->~T();
        full_ = false;
        tail_ = (tail_ + 1) % capacity;

        return val;
    }


private:
    struct Cell
    {
        alignas(alignof(T)) char mem_[sizeof(T)];
    };

    Cell memory_[capacity];
    u32 head_;
    u32 tail_;
    bool full_;
};
