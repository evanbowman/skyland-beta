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

#include "skyland/skyland.hpp"
#include "skyland/systemString.hpp"
#include "worldScene.hpp"



namespace skyland
{



class SelectWeaponGroupScene : public ActiveWorldScene
{
public:
    SelectWeaponGroupScene(DeferredScene cancel) : cancel_(cancel)
    {
    }


    void enter(Scene& prev) override
    {
        ActiveWorldScene::enter(prev);

        text_.emplace(SYSTR(modifier_keys_opt_5)->c_str(),
                      OverlayCoord{0, u8(calc_screen_tiles().y - 1)});

        text_->append(" ");

        PLATFORM.set_tile(
            Layer::overlay, text_->len(), calc_screen_tiles().y - 1, 395);

        PLATFORM.set_tile(
            Layer::overlay, text_->len() + 1, calc_screen_tiles().y - 1, 393);

        PLATFORM.set_tile(
            Layer::overlay, text_->len() + 2, calc_screen_tiles().y - 1, 394);

        for (int i = 0; i < text_->len() + 3; ++i) {
            PLATFORM.set_tile(Layer::overlay, i, 18, 425);
        }
    }



    void exit(Scene& prev) override
    {
        ActiveWorldScene::exit(prev);

        text_.reset();

        PLATFORM.fill_overlay(0);
    }



    ScenePtr update(Time delta) override
    {
        if (auto next = ActiveWorldScene::update(delta)) {
            return next;
        }

        if (APP.player().key_down(Key::up)) {
            for (auto& room : APP.player_island().rooms()) {
                if (room->group() == Room::Group::one) {
                    if (auto scene = room->select(room->position())) {
                        return scene;
                    }
                }
            }
            return cancel_();
        } else if (APP.player().key_down(Key::right)) {
            for (auto& room : APP.player_island().rooms()) {
                if (room->group() == Room::Group::two) {
                    if (auto scene = room->select(room->position())) {
                        return scene;
                    }
                }
            }
            return cancel_();
        } else if (APP.player().key_down(Key::left)) {
            for (auto& room : APP.player_island().rooms()) {
                if (room->group() == Room::Group::three) {
                    if (auto scene = room->select(room->position())) {
                        return scene;
                    }
                }
            }
            return cancel_();
        } else if (APP.player().key_down(Key::action_2)) {
            return cancel_();
        }

        return null_scene();
    }

private:
    DeferredScene cancel_;
    Optional<Text> text_;
};



} // namespace skyland
