////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "radialTransitionAnimScene.hpp"



namespace skyland
{



RadialTransitionAnim::RadialTransitionAnim(DeferredScene next,
                                           int abs_x,
                                           int abs_y,
                                           bool far,
                                           Time duration)
    : duration_(duration), next_(next), abs_x_(abs_x), abs_y_(abs_y)
{
    disable_ui();
    disable_gamespeed_icon();
    if (far) {
        far_camera();
    }
}


void RadialTransitionAnim::enter(Scene& prev)
{
    ActiveWorldScene::enter(prev);
}


void RadialTransitionAnim::exit(Scene& next)
{
    ActiveWorldScene::exit(next);
}


ScenePtr RadialTransitionAnim::update(Time delta)
{
    if (auto scene = ActiveWorldScene::update(delta)) {
        return scene;
    }

    effect_radius_ = ease_in(timer_, 0, 240, duration_);

    timer_ += delta;
    if (timer_ >= duration_) {
        effect_radius_ = 0;
        PLATFORM.screen().schedule_fade(1);
        return next_();
    }

    return null_scene();
}


void RadialTransitionAnim::display()
{
    ActiveWorldScene::display();

    PLATFORM_EXTENSION(
        overlay_circle_effect, effect_radius_, abs_x_, abs_y_, 1);
}



ScenePtr radial_transition_at(Island* isle,
                              Vec2<u8> cursor,
                              bool far_camera,
                              DeferredScene next)
{
    auto pos = isle->visual_origin();
    int x = (pos.x.as_integer() + cursor.x * 16 + 16) -
            PLATFORM.screen().get_view().get_center().x;
    int y = (pos.y.as_integer() + cursor.y * 16 + 16) -
            PLATFORM.screen().get_view().get_center().y - 510;
    x -= 8;
    y -= 8;
    return make_scene<RadialTransitionAnim>(next, x, y, far_camera);
}



} // namespace skyland
