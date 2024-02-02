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

#include <new>
#include <utility>



#if 0


struct NullOpt
{
};



class OptionalBase
{
public:
    static void bad_access_error();
};



template <typename T>
class Optional : public OptionalBase
{
public:

#define CHECK_INIT() if (not initialized_) bad_access_error();


    Optional() = default;


    constexpr Optional(const NullOpt& n) : initialized_(false)
    {
    }


    constexpr Optional(const T& v) : initialized_(true)
    {
        new (v_) T(v);
    }


    constexpr Optional(T&& v) : initialized_(true)
    {
        new (v_) T(std::forward<T>(v));
    }


    bool operator==(const Optional& other) const
    {
        return initialized_ == other.initialized_ and *this == *other;
    }


    Optional(Optional&& other)
    {
        if (other.initialized_) {
            new (v_) T(std::move(*((T*)other.v_)));
            other.destroy();
            initialized_ = true;
        } else {
            initialized_ = false;
        }
    }


    Optional(const Optional& other)
    {
        if (other.initialized_) {
            initialized_ = true;
            new (v_) T(*other);
        } else {
            initialized_ = false;
        }
    }


    Optional& operator=(T&& v)
    {
        destroy();
        new (v_) T(std::forward<T>(v));
        initialized_ = true;
        return *this;
    }


    Optional& operator=(const T& v)
    {
        destroy();
        new (v_) T(v);
        initialized_ = true;
        return *this;
    }


    Optional& operator=(const NullOpt& v)
    {
        destroy();
        initialized_ = false;
        return *this;
    }


    Optional& operator=(const Optional& other)
    {
        destroy();
        if (other.initialized_) {
            new (v_) T(*other);
            initialized_ = true;
        } else {
            initialized_ = false;
        }
        return *this;
    }


    Optional& operator=(Optional&& other)
    {
        destroy();
        if (other.initialized_) {
            new (v_) T(std::move(*other));
            other.destroy();
            initialized_ = true;
        } else {
            initialized_ = false;
        }
        return *this;
    }


    template <typename ...Args>
    void emplace(Args&& ...args)
    {
        destroy();
        new (v_) T(std::forward<Args>(args)...);
        initialized_ = true;
    }


    T release()
    {
        CHECK_INIT();
        T temp = std::move(*((T*)v_));
        destroy();
        return temp;
    }


    constexpr T& operator*() const
    {
        CHECK_INIT();
        return *((T*)v_);
    }


    constexpr T* operator->() const
    {
        CHECK_INIT();
        return (T*)v_;
    }


    operator bool() const
    {
        return initialized_;
    }


    void reset()
    {
        destroy();
    }


    constexpr ~Optional()
    {
        destroy();
    }


private:

    constexpr void destroy()
    {
        if (initialized_) {
            ((T*)v_)->~T();
        }
        initialized_ = false;
    }

    alignas(alignof(T)) char v_[sizeof(T)];
    bool initialized_ = false;
};
#else

#include <optional>

template <typename T>
using Optional = std::optional<T>;


inline auto nullopt()
{
    return std::nullopt;
}


#endif
