////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once


#include "skyland/coins.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/systemString.hpp"
#include "weapon.hpp"



namespace skyland
{



class Annihilator : public Weapon
{
public:
    Annihilator(Island* parent, const RoomCoord& position);


    void fire(Platform& pfrm, App& app) override;
    Microseconds reload() const override;


    void render_interior(App& app, TileId buffer[16][16]) override;
    void render_exterior(App& app, TileId buffer[16][16]) override;


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer);


    static Category category()
    {
        return Category::weapon;
    }


    bool description_visible() override
    {
        return true;
    }


    static Vec2<u8> size()
    {
        return {1, 2};
    }


    static const char* name()
    {
        return "annihilator";
    }


    static SystemString ui_name()
    {
        return SystemString::block_annihilator;
    }



    static Float ai_base_weight()
    {
        return 1200.f;
    }


    void plot_walkable_zones(App& app, bool matrix[16][16]) override
    {
        // one cannot walk through this tile, intentionally do nothing.
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::workshop_required | RoomProperties::roof_hidden |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::manufactory_required |
               RoomProperties::locked_by_default;
    }


    static Icon icon()
    {
        return 3352;
    }


    static Icon unsel_icon()
    {
        return 3368;
    }
};


} // namespace skyland