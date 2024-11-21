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

#include "number/numeric.hpp"



// A time-driven scheduler. TODO: Optimize this class further.



class Platform;



namespace skyland
{



class App;



class Timer
{
public:
    virtual ~Timer()
    {
    }


    virtual void timer_expired() = 0;


    Time remaining() const
    {
        return clock_;
    }


    void __override_clock(Time value)
    {
        clock_ = value;
    }


    Time interval() const
    {
        return interval_;
    }


private:
    Time clock_ = 0;
    Time interval_ = 0;
    Timer* next_ = nullptr;

    friend class BulkTimer;
};



class BulkTimer
{
public:
    void update(Time elapsed_delta);
    void rewind(Time elapsed_delta);


    void schedule(Timer* subscriber, Time timeout);


    void deschedule(Timer* subscriber);


private:
    Timer* scheduled_ = nullptr;
};



} // namespace skyland
