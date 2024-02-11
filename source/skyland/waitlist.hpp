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

#include "containers/queue.hpp"
#include "function.hpp"



//
// Some computations, primarily AI decisions, need to be scheduled at set
// intervals. But if we just schedule different computations based on a timer,
// some calculations can end up running on the same frame and lagging the
// game. So we have a structure capable of executing deferred units of work,
// called the waitlist. The waitlist smooths out latency spikes by distributing
// units of work across multiple frames.
//



namespace skyland
{



class Waitlist
{
public:
    using Task = Function<2 * sizeof(void*), void()>;


    bool push(Task task);


    Optional<Task> pop();


private:
    Queue<Task, 48> task_queue_;
    int size_ = 0;
};



extern Waitlist waitlist;



} // namespace skyland
