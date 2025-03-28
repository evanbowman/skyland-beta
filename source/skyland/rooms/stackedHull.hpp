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

#include "hull.hpp"
#include "skyland/systemString.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



class StackedHull final : public Hull
{
public:
    StackedHull(Island* parent, const RoomCoord& position);


    using Hull::Hull;


    void update(Time delta) override;
    void rewind(Time delta) override;


    TileId tile() const;


    static const char* name()
    {
        return "stacked-hull";
    }


    void render_scaffolding(TileId buffer[16][16]) override
    {
    }


    static SystemString ui_name()
    {
        return SystemString::block_stacked_hull;
    }


    static void format_description(StringBuffer<512>& buffer)
    {
        buffer += SYSTR(description_stacked_hull)->c_str();
    }


    static RoomProperties::Bitmask properties()
    {
        return Hull::properties() | RoomProperties::disabled_in_tutorials |
               RoomProperties::manufactory_required |
               RoomProperties::multiboot_compatible;
    }


    static Icon icon()
    {
        return 2024;
    }


    static Icon unsel_icon()
    {
        return 2040;
    }


    void render_interior(App* app, TileId buffer[16][16]) override;


    void render_exterior(App* app, TileId buffer[16][16]) override;
};



} // namespace skyland
