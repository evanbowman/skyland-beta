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


#include "nemesis.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/misc/animatedEffect.hpp"
#include "skyland/entity/projectile/nemesisBlast.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



extern Sound cannon_sound;



SHARED_VARIABLE(nemesis_reload_ms);
extern SharedVariable nemesis_blast_damage;



void Nemesis::format_description(StringBuffer<512>& buffer)
{
    auto secs = nemesis_reload_ms / 1000;

    make_format(buffer,
                SYSTR(description_nemesis)->c_str(),
                nemesis_blast_damage,
                nemesis_blast_damage * 2,
                nemesis_blast_damage * 4,
                secs,
                (nemesis_reload_ms / 100 - secs * 10));
}



Nemesis::Nemesis(Island* parent, const RoomCoord& position)
    : Weapon(parent, name(), position, 1000 * nemesis_reload_ms)
{
}



void Nemesis::fire()
{
    auto island = other_island();

    Vec2<Fixnum> target;

    auto origin = island->origin();
    origin.x += Fixnum::from_integer(get_target()->x * 16 + 8);
    origin.y += Fixnum::from_integer(get_target()->y * 16 + 8);
    target = origin;

    APP.camera()->shake(4);

    auto start = center();

    if (is_player_island(island)) {
        start.x -= 22.0_fixed;
    } else {
        start.x += 22.0_fixed;
    }

    if (not PLATFORM.network_peer().is_connected() and
        APP.game_mode() not_eq App::GameMode::tutorial) {
        target = rng::sample<6>(target, rng::critical_state);
    }

    cannon_sound.play(3);

    auto v =
        APP.alloc_entity<NemesisBlast>(start, target, parent(), position());
    if (v) {
        if (health() < max_health() / 4) {
            v->set_variant(2);
        } else if (health() < max_health() / 2) {
            v->set_variant(1);
        }

        parent()->projectiles().push(std::move(v));
    }

    auto e = alloc_entity<AnimatedEffect>(start, 47, 49, milliseconds(100));
    if (e) {
        APP.effects().push(std::move(e));
    }
}



Time Nemesis::reload_impl() const
{
    return 1000 * nemesis_reload_ms;
}



void Nemesis::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::nemesis_1;
    buffer[position().x + 1][position().y] = InteriorTile::nemesis_2;
}



void Nemesis::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::nemesis_1;
    buffer[position().x + 1][position().y] = Tile::nemesis_2;
}



} // namespace skyland
