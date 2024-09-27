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

#include "skyland/save.hpp"
#include "skyland/scene.hpp"
#include "graphics/overlay.hpp"
#include "newgameScene.hpp"
#include "titleScreenScene.hpp"
#include "localization.hpp"



namespace skyland
{



class SaveSlotScene : public Scene
{
public:

    static constexpr const int slot_count_ = 3;


    Optional<save::SlotInfo> slot_info_[3];


    void enter(Scene& prev) override
    {
        for (int i = 0; i < slot_count_; ++i) {
            slot_info_[i] = save::slot_info(i);

            u8 y_start = i * 6 + 2;

            u32 playtime_secs = 0;
            Coins coins = 0;
            [[maybe_unused]] int zone = 0;

            if (slot_info_[i]) {
                playtime_secs = slot_info_[i]->playtime_seconds_;
                coins = slot_info_[i]->coins_;
                zone = slot_info_[i]->zone_;
            }

            Text::print(format("Slot %", i + 1).c_str(), {2, y_start});

            auto playtime = format_time(playtime_secs, true);
            auto pt_len = utf8::len(playtime.c_str());
            Text::print(playtime.c_str(), {(u8)(27 - pt_len), (u8)(y_start + 2)});

            auto coins_text = format("%@", coins);
            auto c_len = utf8::len(coins_text.c_str());
            Text::print(coins_text.c_str(), {(u8)(27 - c_len), (u8)(y_start)});
        }
        PLATFORM.screen().schedule_fade(0);
        PLATFORM.screen().schedule_fade(1);
    }


    void exit(Scene& next) override
    {
        PLATFORM.fill_overlay(0);
    }


    ScenePtr update(Time delta) override
    {
        player().update(delta);

        auto test_key = [&](Key k) {
            return player().test_key(k, milliseconds(500), milliseconds(100));
        };

        if (key_down<Key::action_1>()) {
            PLATFORM.speaker().play_sound("button_wooden", 3);
            save::bind_slot(selected_);
            return make_scene<NewgameScene>();
        } else if (test_key(Key::action_2)) {
            return make_scene<TitleScreenScene>();
        }

        if (test_key(Key::down)) {

            if (selected_ < slot_count_ - 1) {
                ++selected_;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }

        } else if (test_key(Key::up)) {

            if (selected_ > 0) {
                --selected_;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }

        }

        return null_scene();
    }



private:
    save::SaveSlotNum selected_ = 0;
};



}
