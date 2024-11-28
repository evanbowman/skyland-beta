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


#include "beamGun.hpp"
#include "platform/platform.hpp"
#include "skyland/alloc_entity.hpp"
#include "skyland/entity/misc/animatedEffect.hpp"
#include "skyland/entity/projectile/beam.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



SHARED_VARIABLE(beam_reload_ms);



extern Sound cannon_sound;



void BeamGun::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_beam_gun)->c_str();
}



BeamGun::BeamGun(Island* parent, const RoomCoord& position)
    : Weapon(parent, name(), position, 1000 * beam_reload_ms)
{
}



class BeamSpawner : public Entity
{
public:
    BeamSpawner(const Vec2<Fixnum>& position,
                const Vec2<Fixnum>& target,
                Island* source,
                const RoomCoord& origin_tile)
        : Entity({}), position_(position), target_(target), source_(source),
          origin_tile_(origin_tile)
    {
        sprite_.set_alpha(Sprite::Alpha::transparent);
    }


    void rewind(Time delta) override
    {
        timer_ -= delta;
        if (timer_ < 0) {
            timer_ += milliseconds(32);
            if (--beam_count_ == -1) {
                kill();
            }
        }
    }


    void update(Time delta) override
    {
        timer_ += delta;
        if (timer_ > milliseconds(32)) {
            timer_ -= milliseconds(32);
            if (++beam_count_ < 10) {
                auto c = APP.alloc_entity<Beam>(
                    position_, target_, source_, origin_tile_, beam_count_);
                if (c) {
                    source_->projectiles().push(std::move(c));
                }

            } else {
                kill();

                auto e = alloc_entity<AnimatedEffect>(
                    position_, 47, 49, milliseconds(100));
                if (e) {
                    APP.effects().push(std::move(e));
                }
            }
        }
    }


private:
    const Vec2<Fixnum> position_;
    const Vec2<Fixnum> target_;
    Island* source_;
    Time timer_ = 0;
    int beam_count_ = 0;
    RoomCoord origin_tile_;
};



void BeamGun::fire()
{
    auto island = other_island();

    Vec2<Fixnum> target;

    auto origin = island->origin();
    origin.x += Fixnum::from_integer(get_target()->x * 16 + 8);
    origin.y += Fixnum::from_integer(get_target()->y * 16 + 8);
    target = origin;

    APP.camera()->shake(6);

    auto start = center();

    // This just makes it a bit less likely for cannonballs to
    // run into the player's own buildings, especially around
    // corners.
    if (is_player_island(island)) {
        start.x -= 32.0_fixed;
    } else {
        start.x += 32.0_fixed;
    }

    if (not PLATFORM.network_peer().is_connected() and
        APP.game_mode() not_eq App::GameMode::tutorial) {
        target = rng::sample<4>(target, rng::critical_state);
    }

    cannon_sound.play(3);

    auto c = APP.alloc_entity<BeamSpawner>(start, target, parent(), position());
    if (c) {
        parent()->projectiles().push(std::move(c));
    }
}



Time BeamGun::reload_impl() const
{
    return 1000 * beam_reload_ms;
}



void BeamGun::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::beam_gun_1;
    buffer[position().x + 1][position().y] = Tile::beam_gun_2;
    buffer[position().x + 2][position().y] = Tile::beam_gun_3;
}



void BeamGun::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::beam_gun_1;
    buffer[position().x + 1][position().y] = Tile::beam_gun_2;
    buffer[position().x + 2][position().y] = Tile::beam_gun_3;
}



} // namespace skyland
