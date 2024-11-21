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


#include "mycelium.hpp"
#include "skyland/island.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/scene/constructionScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



Mycelium::Mycelium(Island* parent, const RoomCoord& position, const char* n)
    : Room(parent, n, position)
{
    parent->bulk_timer().schedule(this, flood_time);
}



Mycelium::~Mycelium()
{
    parent()->bulk_timer().deschedule(this);
}



void Mycelium::timer_expired()
{
    if (parent()->is_destroyed()) {
        return;
    }

    const auto x = position().x;
    const auto y = position().y;

    if (parent()->fire_present({x, y})) {
        // Do not grow if we're burning.
        parent()->bulk_timer().schedule(this, flood_time);
        return;
    }

    auto substrate = [&](u8 x, u8 y) {
        if (auto room = parent()->get_room({x, y})) {
            // Mycelium substrate must be non-mycelium room.
            return room->metaclass() not_eq metaclass() and
                   not((*room->metaclass())->properties() &
                       RoomProperties::fluid) and
                   not is_forcefield(room->metaclass());
        }

        return false;
    };

    if (not substrate(x + 1, y) and not substrate(x - 1, y) and
        not substrate(x, y - 1) and not substrate(x, y + 1) and
        not substrate(x + 1, y + 1) and not substrate(x - 1, y - 1) and
        not substrate(x + 1, y - 1) and not substrate(x - 1, y + 1) and
        y not_eq 14) {
        this->apply_damage(health_upper_limit());
        return;
    }

    auto slot_valid = [&](u8 x, u8 y) {
        if (x > parent()->terrain().size() - 1 or y > 14 or
            y < construction_zone_min_y) {
            return false;
        }
        if (not parent()->get_room({x, y})) {
            if (y == 14) {
                return true;
            }


            return substrate(x + 1, y) or substrate(x - 1, y) or
                   substrate(x, y - 1) or substrate(x, y + 1) or
                   substrate(x + 1, y + 1) or substrate(x - 1, y - 1) or
                   substrate(x + 1, y - 1) or substrate(x - 1, y + 1);
        }
        return false;
    };

    auto spread = [&](u8 x, u8 y) {
        (*metaclass())->create(parent(), {x, y}, false);

        parent()->schedule_repaint();

        if (is_player_island(parent())) {
            time_stream::event::PlayerRoomCreated p;
            p.x_ = x;
            p.y_ = y;
            APP.time_stream().push(APP.level_timer(), p);
        } else {
            time_stream::event::OpponentRoomCreated p;
            p.x_ = x;
            p.y_ = y;
            APP.time_stream().push(APP.level_timer(), p);
        }
    };

    auto try_spread = [&](u8 x, u8 y) {
        if (slot_valid(x, y)) {
            spread(x, y);
        }
    };

    try_spread(x + 1, y);
    try_spread(x - 1, y);
    try_spread(x, y + 1);
    try_spread(x, y - 1);
    try_spread(x + 1, y + 1);
    try_spread(x - 1, y - 1);
    try_spread(x - 1, y + 1);
    try_spread(x + 1, y - 1);

    parent()->bulk_timer().schedule(this, flood_time);
}



void Mycelium::update(Time delta)
{
    Room::update(delta);
}



void Mycelium::render_interior(App* app, TileId buffer[16][16])
{
    bool above = false;
    bool below = false;

    if (auto room = parent()->get_room({position().x, u8(position().y - 1)})) {
        if (room->metaclass() == metaclass()) {
            above = true;
        }
    }

    if (auto room = parent()->get_room({position().x, u8(position().y + 1)})) {
        if (room->metaclass() == metaclass()) {
            below = true;
        }
    }

    if (above and below) {
        buffer[position().x][position().y] = InteriorTile::mycelium_middle;
    } else if (above) {
        buffer[position().x][position().y] = InteriorTile::mycelium_bottom;
    } else if (below) {
        buffer[position().x][position().y] = InteriorTile::mycelium_top;
    } else {
        buffer[position().x][position().y] = InteriorTile::mycelium;
    }
}



void Mycelium::render_exterior(App* app, TileId buffer[16][16])
{
    bool above = false;
    bool below = false;

    if (auto room = parent()->get_room({position().x, u8(position().y - 1)})) {
        if (room->metaclass() == metaclass()) {
            above = true;
        }
    }

    if (auto room = parent()->get_room({position().x, u8(position().y + 1)})) {
        if (room->metaclass() == metaclass()) {
            below = true;
        }
    }

    if (above and below) {
        buffer[position().x][position().y] = Tile::mycelium_middle;
    } else if (above) {
        buffer[position().x][position().y] = Tile::mycelium_bottom;
    } else if (below) {
        buffer[position().x][position().y] = Tile::mycelium_top;
    } else {
        buffer[position().x][position().y] = Tile::mycelium;
    }
}



} // namespace skyland
