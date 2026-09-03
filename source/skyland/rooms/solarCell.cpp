////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "solarCell.hpp"
#include "skyland/island.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"
#include "skyland/weather/solarStorm.hpp"



namespace skyland
{



int SolarCell::factor_power(int value) const
{
    if (APP.environment().id() == weather::SolarStorm::id_) {
        return value * 2;
    } else if (APP.environment().is_overcast()) {
        return value / 2;
    } else {
        if (APP.environment().is_night()) {
            return 0;
        }
    }

    return value;
}



Power SolarCell::power_usage() const
{
    const auto base_power = (*metaclass())->consumes_power();

    return factor_power(base_power);
}



void SolarCell::update(Time delta)
{
    Room::update(delta);

    if (hint_release_cyc_) {
        hint_release_cyc_--;
        if (hint_release_cyc_ == 0) {
            power_hint_.reset();
        }
    }

    if (power_hint_) {
        Room::ready();
    }
}



void SolarCell::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_solar_cell)->c_str();
}



void SolarCell::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::solar_cell;
    buffer[position().x + 1][position().y] = InteriorTile::solar_cell;
}



void SolarCell::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::solar_cell;
    buffer[position().x + 1][position().y] = Tile::solar_cell;
}



void SolarCell::display_on_hover(Platform::Screen& screen,
                                 const RoomCoord& cursor)
{
    if (factor_power(10) == 10) {
        return;
    }

    if (not power_hint_ and ++hint_show_cyc_ > 5) {
        SpriteText::Configuration conf{.shade_bg_index_ = 3,
                                       .shade_fg_index_ = 2};

        hint_env_id_ = APP.environment().id();
        auto msg = format(SYS_CSTR(solar_power_hint), factor_power(10));
        auto mem = allocate<SpriteText>({"hint-spr-text"}, msg.c_str(), conf);
        power_hint_ = std::move(mem);
        (*power_hint_)->set_palette(1);
        auto p = visual_center();
        hint_x_ = p.x - Fixnum::from_integer((8 * utf8::len(msg.c_str())) / 2);
        hint_show_cyc_ = 0;
    } else if (hint_env_id_ not_eq APP.environment().id()) {
        power_hint_.reset();
    }

    if (power_hint_) {
        Vec2<Fixnum> pos;
        pos.x = hint_x_;
        if (pos.x.as_integer() < PLATFORM.screen().get_view().int_center().x) {
            pos.x = PLATFORM.screen().get_view().int_center().x;
        }
        pos.y = visual_center().y - 16.0_fixed;
        (*power_hint_)->set_position(pos);
        (*power_hint_)->draw();
        hint_release_cyc_ = 3;

        Room::ready();
    }
}



} // namespace skyland
