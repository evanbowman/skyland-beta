////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman. Some rights reserved.
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

#include "numeric.hpp"
#include "platform/libc.hpp"
#include <bit>


// Because most processors are little endian, I am using little endian byte
// order for binary encoded data.


#ifdef __bswap_constant_16
#undef __bswap_constant_16
#endif
#define __bswap_constant_16(x)                                                 \
    ((u16)((((x) >> 8) & 0xff) | (((x) & 0xff) << 8)))

#ifdef __bswap_constant_32
#undef __bswap_constant_32
#endif
#define __bswap_constant_32(x)                                                 \
    ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >> 8) |                  \
     (((x) & 0x0000ff00) << 8) | (((x) & 0x000000ff) << 24))

#ifdef __bswap_constant_64
#undef __bswap_constant_64
#endif
#define __bswap_constant_64(x)                                                 \
    ((((x) & 0xff00000000000000ull) >> 56) |                                   \
     (((x) & 0x00ff000000000000ull) >> 40) |                                   \
     (((x) & 0x0000ff0000000000ull) >> 24) |                                   \
     (((x) & 0x000000ff00000000ull) >> 8) |                                    \
     (((x) & 0x00000000ff000000ull) << 8) |                                    \
     (((x) & 0x0000000000ff0000ull) << 24) |                                   \
     (((x) & 0x000000000000ff00ull) << 40) |                                   \
     (((x) & 0x00000000000000ffull) << 56))


#if defined(__GBA__) or defined(__NDS__)
inline constexpr bool is_little_endian()
{
    return true;
}
#else
inline bool is_little_endian()
{
    // FIXME: use std::endian
    static const u16 i = 0x000f;
    return ((u8*)&i)[0] == 0x0f;
}
#endif



template <typename T> T to_host_order(T value);


template <> inline u8 to_host_order(u8 val)
{
    return val;
}

template <> inline u16 to_host_order(u16 val)
{
    if (is_little_endian()) {
        return val;
    } else {
        return __bswap_constant_16(val);
    }
}

template <> inline s16 to_host_order(s16 val)
{
    if (is_little_endian()) {
        return val;
    } else {
        return __bswap_constant_16(val);
    }
}

template <> inline u32 to_host_order(u32 val)
{
    if (is_little_endian()) {
        return val;
    } else {
        return __bswap_constant_32(val);
    }
}

template <> inline s32 to_host_order(s32 val)
{
    if (is_little_endian()) {
        return val;
    } else {
        return __bswap_constant_32(val);
    }
}

template <> inline u64 to_host_order(u64 val)
{
    if (is_little_endian()) {
        return val;
    } else {
        return __bswap_constant_64(val);
    }
}

template <> inline s64 to_host_order(s64 val)
{
    if (is_little_endian()) {
        return val;
    } else {
        return __bswap_constant_64(val);
    }
}


template <typename T> class HostInteger
{
public:
    // NOTE: I could overload the cast operator and assignment operators to make
    // this class behave similarly to a plain integer. But then I get
    // class-memaccess errors, which I don't want to disable throughout the
    // project... calling get()/set() isn't so bad.

    HostInteger() = default;

    explicit HostInteger(T value)
    {
        set(value);
    }

    void set(T value)
    {
        value = to_host_order(value);
        memcpy(data_, &value, sizeof(T));
    }

    T get() const
    {
        T value;
        memcpy(&value, data_, sizeof(T));
        return to_host_order(value);
    }

private:
    char data_[sizeof(T)];
};


using host_u16 = HostInteger<u16>;
using host_u32 = HostInteger<u32>;
using host_s16 = HostInteger<s16>;
using host_s32 = HostInteger<s32>;
using host_u64 = HostInteger<u64>;
using host_s64 = HostInteger<s64>;
