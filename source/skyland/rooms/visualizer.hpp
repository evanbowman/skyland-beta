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

#include "canvas.hpp"
#include "decoration.hpp"
#include "platform/platform.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class Visualizer final : public Canvas
{
public:
    static void format_description(StringBuffer<512>& buffer);


    Visualizer(Island* parent, const RoomCoord& position);
    ~Visualizer();


    void update(Time delta) override;
    void rewind(Time delta) override;


    void update_simple(Time delta);


    static RoomProperties::Bitmask properties()
    {
        return (Decoration::properties() |
                RoomProperties::disabled_in_tutorials);
    }


    static const constexpr char* name()
    {
        return "visualizer";
    }


    static SystemString ui_name()
    {
        return SystemString::block_visualizer;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 4424;
    }


    static Icon unsel_icon()
    {
        return 4408;
    }


    ScenePtr select_impl(const RoomCoord& cursor) override
    {
        return null_scene();
    }


    lisp::Value* serialize() override
    {
        return Room::serialize();
    }


    void deserialize(lisp::Value* v) override
    {
        Room::deserialize(v);
    }


    void register_select_menu_options(SelectMenuScene& sel) override
    {
    }

    static void psg_note_on(Platform::Speaker::Channel ch,
                            Platform::Speaker::NoteDesc note,
                            Platform::Speaker::ChannelSettings settings);


    static void psg_stop();

private:
    bool bind_texture();
    void draw(u32 packed);

    Platform::FileSlice spectrum_file_;
    u32 last_music_checksum_ = 0;
};



} // namespace skyland
