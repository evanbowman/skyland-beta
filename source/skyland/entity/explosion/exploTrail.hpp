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

#include "skyland/entity.hpp"



namespace skyland
{



class ExploTrail : public Entity
{
public:
    ExploTrail(const Vec2<Fixnum>& pos, int angle, Fixnum speed, Time duration);


    void update(Time delta) override;


    void rewind(Time delta) override
    {
        kill();
    }


    ~ExploTrail()
    {
        --s_count_;
    }


    bool entity_oom_deletable() const override
    {
        return false;
    }


private:
    Time duration_;
    Vec2<Fixnum> speed_;
    Time timer1_ = 0;
    Time timer2_ = 0;

    static u8 s_count_;
};



} // namespace skyland
