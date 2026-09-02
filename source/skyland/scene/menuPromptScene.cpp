////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2026 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#include "menuPromptScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/player/player.hpp"



namespace skyland
{



void MenuPromptScene::enter(Scene& prev)
{
    PLATFORM.screen().schedule_fade(0);
    PLATFORM.screen().schedule_fade(1);

    text_.emplace();
    text_->assign(msg_->c_str(), {1, 1}, {28, 14}, 0);

    t1_.emplace(OverlayCoord{3, 16});

    t1_->assign(loadstr(opt_1_)->c_str(), sel_colors);

    t2_.emplace(loadstr(opt_2_)->c_str(), OverlayCoord{3, 18});

    PLATFORM.set_overlay_tile(1, 16, 475);
    PLATFORM.set_overlay_tile(1, 18, 0);

    if (play_alert_sfx_) {
        PLATFORM.speaker().play_sound("click_digital_1", 1);
    }
}



void MenuPromptScene::exit(Scene& next)
{
    text_.reset();
    t1_.reset();
    t2_.reset();

    PLATFORM.fill_overlay(0);

    if (not skip_unfade_) {
        PLATFORM.screen().schedule_fade(0.f);
    }
}



ScenePtr MenuPromptScene::update(Time delta)
{
    if (player().button_down(Button::action_1)) {
        if (cursor_ == 0) {
            opt_1_callback_();
        } else {
            opt_2_callback_();
        }
        PLATFORM.speaker().play_sound("button_wooden", 3);
        return next_();
    }

    if (player().button_down(Button::up)) {
        cursor_ = 0;
        t1_->assign(loadstr(opt_1_)->c_str(), sel_colors);
        t2_->assign(loadstr(opt_2_)->c_str());
        PLATFORM.set_overlay_tile(1, 16, 475);
        PLATFORM.set_overlay_tile(1, 18, 0);
        PLATFORM.speaker().play_sound("cursor_tick", 0);
    }

    if (player().button_down(Button::down)) {
        cursor_ = 1;
        t1_->assign(loadstr(opt_1_)->c_str());
        t2_->assign(loadstr(opt_2_)->c_str(), sel_colors);
        PLATFORM.set_overlay_tile(1, 18, 475);
        PLATFORM.set_overlay_tile(1, 16, 0);
        PLATFORM.speaker().play_sound("cursor_tick", 0);
    }

    return null_scene();
}



ScenePtr simple_prompt_once(GlobalPersistentData::Flags check_flag,
                            StateBit runtime_flag,
                            const char* prompt,
                            DeferredScene next)
{
    const bool skip_prompt = APP.gp_.stateflags_.get(check_flag) or
        state_bit_load(runtime_flag) or
        APP.game_mode() == App::GameMode::tutorial;

    auto dont_remind = [flag = check_flag]() {
        APP.gp_.stateflags_.set(flag, true);
        save::store_global_data(APP.gp_);
    };

    if (not skip_prompt) {
        state_bit_store(runtime_flag, true);
        return make_scene<MenuPromptScene>(prompt,
                                           SystemString::ok,
                                           SystemString::do_not_show_again,
                                           next,
                                           []() {},
                                           dont_remind);
    } else {
        return null_scene();
    }
}



}
