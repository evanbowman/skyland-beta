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


#include "warEngine.hpp"
#include "platform/platform.hpp"
#include "skyland/entity/explosion/coreExplosion.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void WarEngine::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_war_engine)->c_str();
}



WarEngine::WarEngine(Island* parent, const RoomCoord& position, const char* n)
    : Room(parent, n, position)
{
}



void WarEngine::update(Time delta)
{
    Room::update(delta);
}



extern Sound core_destroyed;



void WarEngine::finalize()
{
    Room::finalize();

    if (health() == 0) {
        core_destroyed.play(4, milliseconds(600));
        core_explosion(parent(), center(), CoreExplosionConfig{.arms_ = 6});
    }
}



void WarEngine::plot_walkable_zones(bool matrix[16][16],
                                    BasicCharacter* for_character)
{
    auto x = position().x;
    auto y = position().y;

    matrix[x][y + 2] = true;
    matrix[x][y + 3] = true;
    matrix[x + 1][y + 3] = true;
    matrix[x + 2][y + 2] = true;
    matrix[x + 2][y + 3] = true;
}



void WarEngine::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::war_engine_1;
    buffer[position().x + 1][position().y] = InteriorTile::war_engine_2;
    buffer[position().x + 2][position().y] = InteriorTile::war_engine_3;
    buffer[position().x][position().y + 1] = InteriorTile::war_engine_4;
    buffer[position().x + 1][position().y + 1] = InteriorTile::war_engine_5;
    buffer[position().x + 2][position().y + 1] = InteriorTile::war_engine_6;
    buffer[position().x][position().y + 2] = InteriorTile::war_engine_7;
    buffer[position().x + 1][position().y + 2] = InteriorTile::war_engine_8;
    buffer[position().x + 2][position().y + 2] = InteriorTile::war_engine_9;
    buffer[position().x][position().y + 3] = InteriorTile::ladder_base;
    buffer[position().x + 1][position().y + 3] = InteriorTile::plain_floor;
    buffer[position().x + 2][position().y + 3] = InteriorTile::ladder_base;
}



void WarEngine::render_exterior(App* app, TileId buffer[16][16])
{
    auto x = position().x;
    auto y = position().y;

    buffer[x][y] = Tile::wall_window_1;
    buffer[x][y + 1] = Tile::wall_window_middle_2;
    buffer[x][y + 2] = Tile::wall_window_middle_1;
    buffer[x][y + 3] = Tile::wall_window_2;

    buffer[x + 1][y] = Tile::wall_window_1;
    buffer[x + 1][y + 1] = Tile::wall_window_middle_2;
    buffer[x + 1][y + 2] = Tile::wall_window_middle_1;
    buffer[x + 1][y + 3] = Tile::wall_window_2;

    buffer[x + 2][y] = Tile::wall_window_1;
    buffer[x + 2][y + 1] = Tile::wall_window_middle_2;
    buffer[x + 2][y + 2] = Tile::wall_window_middle_1;
    buffer[x + 2][y + 3] = Tile::wall_window_2;
}



} // namespace skyland
