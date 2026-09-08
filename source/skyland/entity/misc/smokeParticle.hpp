////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#pragma once

#include "animatedEffect.hpp"


namespace skyland
{



class SmokeParticle : public AnimatedEffect
{
public:

    SmokeParticle(const Vec2<Fixnum>& pos);


    void update(Time delta) override;


    void rewind(Time delta) override;


    void move(Angle dir, Fixnum speed);


    void set_color(ColorConstant c);


    static void spawn(const Vec2<Fixnum>& pos,
                      int n = 1,
                      Optional<ColorConstant> color = nullopt());

public:
    Vec2<Fixnum> velocity_;
    bool ignore_gamespeed_ = false;
};



}
