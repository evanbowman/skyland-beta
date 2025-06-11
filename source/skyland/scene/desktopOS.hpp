////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "skyland/scene.hpp"



namespace skyland
{



void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



class DesktopOS : public Scene
{
public:

    void draw_menu_bar()
    {
        for (int x = 0; x < 30; ++x) {
            PLATFORM.set_tile(Layer::overlay, x, 0, 82);
            PLATFORM.set_tile(Layer::overlay, x, 1, 82);
        }
        PLATFORM.set_overlay_origin(0, 7);

        Text::print("File", {2, 1});
        Text::print("Edit", {8, 1});

        PLATFORM.set_tile(Layer::overlay, 0, 1, 84);

    }


    void draw_dock()
    {
        for (int x = 0; x < 24; ++x) {
            for (int y = 0; y < 3; ++y) {
                PLATFORM.set_tile(Layer::overlay, x + 3, y + 18, 83);
            }
        }
        PLATFORM.set_tile(Layer::overlay, 3, 18, 85); // corners
        PLATFORM.set_tile(Layer::overlay, 26, 18, 86);
    }


    void enter(Scene&) override
    {
        PLATFORM.load_sprite_texture("spritesheet_os");

        PLATFORM.speaker().stop_music();
        PLATFORM.load_tile0_texture("wallpaper_flattened");
        for (int x = 0; x < 30; ++x) {
            for (int y = 0; y < 20; ++y) {
                PLATFORM.set_raw_tile(Layer::map_0, x, y, 32);
            }
        }
        __draw_image(1, 0, 1, 30, 16, Layer::map_0);
        PLATFORM.screen().schedule_fade(0);

        PLATFORM.fill_overlay(0);
        PLATFORM.load_overlay_texture("overlay_os");

        draw_menu_bar();
        draw_dock();

        PLATFORM.screen().set_view({});
        PLATFORM.screen().schedule_fade(0.5f);
        PLATFORM.screen().schedule_fade(0);

        cursor_ = {116.0_fixed, 76.0_fixed};

        dock_icons_.push_back(13);
        dock_icons_.push_back(14);
    }


    void exit(Scene&) override
    {
    }


    ScenePtr update(Time delta) override
    {
        player().update(delta);

        if (player().key_pressed(Key::down)) {
            if (cursor_.y < 159.0_fixed) {
                cursor_.y += 1.2_fixed;
            }
        }
        if (player().key_pressed(Key::up)) {
            if (cursor_.y > 0.0_fixed) {
                cursor_.y -= 1.2_fixed;
            }
        }

        if (player().key_pressed(Key::right)) {
            if (cursor_.x < 239.0_fixed) {
                cursor_.x += 1.2_fixed;
            }
        }
        if (player().key_pressed(Key::left)) {
            if (cursor_.x > 0.0_fixed) {
                cursor_.x -= 1.2_fixed;
            }
        }


        return null_scene();
    }


    void display() override
    {
        Sprite spr;
        spr.set_size(Sprite::Size::w16_h32);
        spr.set_texture_index(12);
        spr.set_position(cursor_);
        spr.set_priority(0);
        PLATFORM.screen().draw(spr);

        Vec2<Fixnum> icon_pos;
        icon_pos.x = 30.0_fixed;
        icon_pos.y = 139.0_fixed;
        for (auto& ico : dock_icons_) {
            spr.set_texture_index(ico);
            spr.set_position(icon_pos);
            icon_pos.x += 24.0_fixed;
            PLATFORM.screen().draw(spr);
        }
    }


private:
    Vec2<Fixnum> cursor_;
    Buffer<u8, 8> dock_icons_;
};



}
