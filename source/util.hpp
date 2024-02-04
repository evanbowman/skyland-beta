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

#define COLD [[gnu::cold]]
#define HOT [[gnu::hot]]

#include <initializer_list>

#ifdef __GNUC__
#define UNLIKELY(COND) __builtin_expect((COND), false)
#else
#define UNLIKELY(COND) (COND)
#endif


#if defined(__GBA__) or defined(__NDS__)
#define READ_ONLY_DATA __attribute__((section(".rodata")))
#else
#define READ_ONLY_DATA
#endif



template <typename F>
void foreach_reversed(auto& iterable, F&& callback)
{
    auto end = iterable.end();
    auto first = iterable.begin();

    if (end == first) {
        return;
    }

    --end;

    for (;;) {

        callback(*end);

        if (end == iterable.begin()) {
            break;
        } else {
            --end;
        }
    }
}



template <typename T, typename U> bool contains(const T& t, const U& u)
{
    for (auto& v : t) {
        if (v == u) {
            return true;
        }
    }
    return false;
}



void logic_error(const char* file, int line);
#define LOGIC_ERROR() logic_error(__FILE__, __LINE__)


namespace util
{



template <class T> const T& max(const T& a, const T& b)
{
    return (a < b) ? b : a;
}



template <class ForwardIt>
ForwardIt max_element(ForwardIt first, ForwardIt last)
{
    if (first == last)
        return last;

    ForwardIt largest = first;

    while (++first != last)
        if (*largest < *first)
            largest = first;

    return largest;
}



template <class T> T max(std::initializer_list<T> ilist)
{
    return *max_element(ilist.begin(), ilist.end());
}



template <class T> const T& min(const T& a, const T& b)
{
    return (b < a) ? b : a;
}



template <class ForwardIt>
ForwardIt min_element(ForwardIt first, ForwardIt last)
{
    if (first == last)
        return last;

    ForwardIt smallest = first;

    while (++first != last)
        if (*first < *smallest)
            smallest = first;

    return smallest;
}



template <class T> T min(std::initializer_list<T> ilist)
{
    return *min_element(ilist.begin(), ilist.end());
}



} // namespace util
