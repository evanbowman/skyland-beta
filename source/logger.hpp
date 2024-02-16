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

#include "containers/vector.hpp"
#include "severity.hpp"



void log_write(Severity s, const char* msg);



void log_flush();



void log_clear();



Vector<char>* log_data();



inline void debug(const char* msg)
{
    log_write(Severity::debug, msg);
}
inline void info(const char* msg)
{
    log_write(Severity::info, msg);
}
inline void warning(const char* msg)
{
    log_write(Severity::warning, msg);
}
inline void error(const char* msg)
{
    log_write(Severity::error, msg);
}

template <u32 size> void debug(const StringBuffer<size>& buffer)
{
    log_write(Severity::debug, buffer.c_str());
}
template <u32 size> void info(const StringBuffer<size>& buffer)
{
    log_write(Severity::debug, buffer.c_str());
}
template <u32 size> void warning(const StringBuffer<size>& buffer)
{
    log_write(Severity::debug, buffer.c_str());
}
template <u32 size> void error(const StringBuffer<size>& buffer)
{
    log_write(Severity::debug, buffer.c_str());
}
