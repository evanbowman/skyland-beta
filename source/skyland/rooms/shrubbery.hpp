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


#include "decoration.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class Shrubbery final : public Decoration
{
public:
    Shrubbery(Island* parent, const RoomCoord& position)
        : Decoration(parent, name(), position)
    {
    }


    static void format_description(Platform& pfrm, StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_shrubbery)->c_str();
    }


    void render_interior(App& app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::shrubbery;
    }


    void render_exterior(App& app, TileId buffer[16][16]) override
    {
        buffer[position().x][position().y] = Tile::shrubbery;
    }


    static RoomProperties::Bitmask properties()
    {
        return (Decoration::properties() & ~RoomProperties::locked_by_default) |
               RoomProperties::highly_flammable;
    }


    static const char* name()
    {
        return "shrubbery";
    }


    static SystemString ui_name()
    {
        return SystemString::block_shrubbery;
    }


    static Vec2<u8> size()
    {
        return {1, 1};
    }


    static Icon icon()
    {
        return 1416;
    }


    static Icon unsel_icon()
    {
        return 1432;
    }
};



} // namespace skyland