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

#include "worldScene.hpp"



namespace skyland
{



class RadialTransitionAnim : public ActiveWorldScene
{
public:
    RadialTransitionAnim(DeferredScene next,
                         int abs_x,
                         int abs_y,
                         bool far,
                         Time duration = milliseconds(180));


    void enter(Scene& prev) override;


    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void display() override;


private:
    Time timer_ = 0;
    Time duration_;
    DeferredScene next_;
    int abs_x_;
    int abs_y_;
    int effect_radius_ = 0;
};



ScenePtr radial_transition_at(Island* isle,
                              Vec2<u8> cursor,
                              bool far_camera,
                              DeferredScene next);



} // namespace skyland
