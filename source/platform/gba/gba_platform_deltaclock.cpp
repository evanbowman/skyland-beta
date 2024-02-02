////////////////////////////////////////////////////////////////////////////////
//
// MIT License
//
// Copyright (c) 2020-2024 Evan Bowman
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in all
// copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.
//
////////////////////////////////////////////////////////////////////////////////


#include "gba.h"
#include "platform/platform.hpp"



Platform::DeltaClock::DeltaClock() : impl_(nullptr)
{
}



static size_t delta_total;



void start_deltaclock()
{
    irqSet(IRQ_TIMER3, [] {
        delta_total += 0xffff;

        REG_TM3CNT_H = 0;
        REG_TM3CNT_L = 0;
        REG_TM3CNT_H = 1 << 7 | 1 << 6;
    });
}



static int delta_read_tics()
{
    return REG_TM3CNT_L + delta_total;
}



static Microseconds delta_convert_tics(int tics)
{
    //
    // IMPORTANT: Already well into development, I discovered that the Gameboy
    // Advance does not refresh at exactly 60 frames per second. Rather than
    // change all of the code, I am going to keep the timestep as-is. Anyone
    // porting the code to a new platform should make the appropriate
    // adjustments in their implementation of DeltaClock. I believe the actual
    // refresh rate on the GBA is something like 59.59.
    //
    // P.S.: Now, I've discovered that the screen refresh rate is actually 59.73
    // Hz. Sorry to have created a headache for anyone in the future who may be
    // attempting to port this game.
    //
    return ((tics * (59.59f / 60.f)) * 60.f) / 1000.f;
}



Platform::DeltaClock::TimePoint Platform::DeltaClock::sample() const
{
    return delta_read_tics();
}



Microseconds Platform::DeltaClock::duration(TimePoint t1, TimePoint t2)
{
    return delta_convert_tics(t2 - t1);
}



static EWRAM_DATA Microseconds last_delta = 0;



Microseconds Platform::DeltaClock::reset()
{
    // (1 second / 60 frames) x (1,000,000 microseconds / 1 second) =
    // 16,666.6...

    irqDisable(IRQ_TIMER3);
    const auto tics = delta_read_tics();
    REG_TM3CNT_H = 0;

    irqEnable(IRQ_TIMER3);

    delta_total = 0;

    REG_TM3CNT_L = 0;
    REG_TM3CNT_H = 1 << 7 | 1 << 6;

    ::last_delta = delta_convert_tics(tics);
    return ::last_delta;
}



Microseconds Platform::DeltaClock::last_delta() const
{
    return ::last_delta;
}



Platform::DeltaClock::~DeltaClock()
{
}
