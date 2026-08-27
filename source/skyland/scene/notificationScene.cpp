////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "notificationScene.hpp"
#include "readyScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



ScenePtr NotificationScene::update(Time delta)
{
    if (auto next = ActiveWorldScene::update(delta)) {
        return next;
    }

    if (APP.player().button_down(Button::action_1) or
        APP.player().button_down(Button::action_2) or
        APP.player().button_down(Button::left) or
        APP.player().button_down(Button::right) or
        APP.player().button_down(Button::up) or
        APP.player().button_down(Button::down)) {

        return next_scene_();
    }

    return null_scene();
}



void NotificationScene::enter(Scene& prev)
{
    ActiveWorldScene::enter(prev);

    description_.emplace();

    u8 place_y = u8(calc_screen_tiles().y - 1);
    u8 box_width = 30;

    auto text_length = utf8::len(msg_.c_str());
    if (text_length < box_width) {
        box_width = text_length;
    }

    const auto lines = description_->assign(
        msg_.c_str(), OverlayCoord{0, place_y}, {box_width, 5}, 0);

    if (lines > 1) {
        description_.emplace();

        place_y -= (lines - 1);

        description_->assign(
            msg_.c_str(), OverlayCoord{0, place_y}, {30, 5}, 0);
    }

    for (int i = 0; i < box_width; ++i) {
        PLATFORM.set_overlay_tile(i, place_y - 1, 425);
    }
}



void NotificationScene::exit(Scene& next)
{
    ActiveWorldScene::exit(next);

    description_.reset();
    PLATFORM.fill_overlay(0);
}



ScenePtr notify_error(SystemString message)
{
    auto future_scene = []() { return make_scene<ReadyScene>(); };
    PLATFORM.speaker().play_sound("beep_error", 2);
    auto str = loadstr(message);
    return make_scene<NotificationScene>(str->c_str(), future_scene);
}



} // namespace skyland
