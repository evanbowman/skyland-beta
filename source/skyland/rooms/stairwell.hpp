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

#include "skyland/coins.hpp"
#include "skyland/room.hpp"
#include "skyland/systemString.hpp"



namespace skyland
{



class Stairwell : public Room
{
public:
    Stairwell(Island* parent,
              const RoomCoord& position,
              const char* n = name());


    void update(Time delta) override;


    static void format_description(StringBuffer<512>& buffer);


    const char* upgrade_mt_name() const override;


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    void plot_walkable_zones(bool matrix[16][16],
                             BasicCharacter* for_character) override;


    static ATP atp_value()
    {
        return 50.0_atp;
    }


    static Vec2<u8> size()
    {
        return {1, 4};
    }


    static const char* name()
    {
        return "stairwell";
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::habitable | RoomProperties::multiboot_compatible;
    }


    static SystemString ui_name()
    {
        return SystemString::block_stairwell;
    }


    static Icon icon()
    {
        return 616;
    }


    static Icon unsel_icon()
    {
        return 600;
    }


    void finalize() override;
};



class StairwellPlus : public Stairwell
{
public:
    StairwellPlus(Island* parent, const RoomCoord& position);


    static void format_description(StringBuffer<512>& buffer);


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    static SystemString ui_name()
    {
        return SystemString::block_stairwell_plus;
    }


    static Vec2<u8> size()
    {
        return {1, 5};
    }


    void plot_walkable_zones(bool matrix[16][16],
                             BasicCharacter* for_character) override;


    static const char* name()
    {
        return "stairwell+";
    }


    const char* upgrade_mt_name() const override;


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::habitable |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::not_constructible;
    }
};



class StairwellPlusPlus : public Stairwell
{
public:
    StairwellPlusPlus(Island* parent, const RoomCoord& position);


    static void format_description(StringBuffer<512>& buffer);


    void render_interior(App* app, TileId buffer[16][16]) override;
    void render_exterior(App* app, TileId buffer[16][16]) override;


    const char* upgrade_mt_name() const override;


    static SystemString ui_name()
    {
        return SystemString::block_stairwell_plus_plus;
    }


    static Vec2<u8> size()
    {
        return {1, 6};
    }


    void plot_walkable_zones(bool matrix[16][16],
                             BasicCharacter* for_character) override;


    static const char* name()
    {
        return "stairwell++";
    }


    static RoomProperties::Bitmask properties()
    {
        return RoomProperties::habitable |
               RoomProperties::disabled_in_tutorials |
               RoomProperties::not_constructible;
    }
};



} // namespace skyland
