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


#include "projectile.hpp"


#include "pluginProjectile.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/sound.hpp"
#include "skyland/tile.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



PluginProjectile::PluginProjectile(const Vec2<Fixnum>& position,
                                   const Vec2<Fixnum>& target,
                                   Island* source,
                                   const RoomCoord& origin_tile,
                                   u16 graphics_tile,
                                   Health damage,
                                   bool flip)
    : Projectile({{10, 10}, {8, 8}}), source_(source),
      origin_tile_(origin_tile), damage_(damage)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);
    sprite_.set_texture_index(graphics_tile);

    if (sprite_.get_texture_index() >= SpriteTile::custom_sprite_tile_begin) {
        sprite_.set_palette(1);
    }

    sprite_.set_origin({8, 8});

    sprite_.set_flip({flip, false});

    static const Float speed = 0.00015f;

    auto step = direction(fvec(position), fvec(target)) * speed;
    step_vector_ = Vec2<Fixnum>{Fixnum(step.x), Fixnum(step.y)};
}



void PluginProjectile::update(Time delta)
{
    auto pos = sprite_.get_position();
    pos = pos + APP.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    timer_ += delta;

    if (timer_ > seconds(2)) {
        kill();
    }
}



void PluginProjectile::rewind(Time delta)
{
    auto pos = sprite_.get_position();
    pos = pos - APP.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    timer_ -= delta;

    if (timer_ < 0) {
        if (auto room = source_->get_room(origin_tile_)) {
            room->___rewind___ability_used();
        } else if (auto drone = source_->get_drone(origin_tile_)) {
            (*drone)->___rewind___ability_used();
        }
        kill();
    }
}



extern Sound sound_impact;



void PluginProjectile::on_collision(Room& room, Vec2<u8> origin)
{
    if (source_ == room.parent()) {
        if (auto origin = source_->get_room(origin_tile_)) {
            if (origin == &room) {
                return;
            }
        }
        if (room.position().x + (room.size().x - 1) == origin_tile_.x) {
            // Because we do not want to include collisions with the originating
            // cannon, or with any blocks directly above or below the cannon.
            return;
        }
    }

    if (source_ == room.parent() and is_forcefield(room.metaclass())) {
        return;
    }

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < damage_) {
        room.apply_damage(Room::health_upper_limit());
        return;
    }

    timestream_record_destroyed();

    kill();
    APP.camera()->shake(8);
    medium_explosion(sprite_.get_position());

    room.apply_damage(damage_);

    if (room.health()) {
        sound_impact.play(1);
    }
}



void PluginProjectile::timestream_record_destroyed()
{
    auto timestream_record =
        [&](time_stream::event::PluginProjectileDestroyed& c) {
            c.x_origin_ = origin_tile_.x;
            c.y_origin_ = origin_tile_.y;
            c.timer_.set(timer_);
            c.x_pos_.set(sprite_.get_position().x.as_integer());
            c.y_pos_.set(sprite_.get_position().y.as_integer());
            c.x_speed__data_.set(step_vector_.x.data());
            c.y_speed__data_.set(step_vector_.y.data());
            c.tile_.set(sprite().get_texture_index());
            c.damage_.set(damage_);
            c.hflip_ = sprite().get_flip().x;
        };


    if (is_player_island(source_)) {
        time_stream::event::PlayerPluginProjectileDestroyed c;
        timestream_record(c);
        APP.time_stream().push(APP.level_timer(), c);
    } else {
        time_stream::event::OpponentPluginProjectileDestroyed c;
        timestream_record(c);
        APP.time_stream().push(APP.level_timer(), c);
    }
}



void PluginProjectile::on_collision(Entity& entity)
{
    timestream_record_destroyed();

    kill();
    APP.camera()->shake(8);
    medium_explosion(sprite_.get_position());

    entity.apply_damage(damage_);
}



} // namespace skyland
