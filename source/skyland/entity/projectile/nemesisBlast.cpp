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


#include "projectile.hpp"


#include "nemesisBlast.hpp"
#include "skyland/entity/drones/drone.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/sound.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



SHARED_VARIABLE(nemesis_blast_damage);



NemesisBlast::NemesisBlast(const Vec2<Fixnum>& position,
                           const Vec2<Fixnum>& target,
                           Island* source,
                           const RoomCoord& origin_tile)
    : Projectile({{10, 10}, {8, 8}}), source_(source), origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);

    sprite_.set_origin({8, 8});

    static const Float speed = 0.00018f;
    auto step = direction(fvec(position), fvec(target)) * speed;
    step_vector_ = Vec2<Fixnum>{step.x, step.y};
}



void NemesisBlast::update(Platform&, App& app, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos + app.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    sprite_.set_texture_index(80 + variant_);

    timer_ += delta;

    if (timer_ > seconds(2)) {
        kill();
    }
}



void NemesisBlast::rewind(Platform& pfrm, App& app, Microseconds delta)
{
    auto pos = sprite_.get_position();
    pos = pos - app.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    sprite_.set_texture_index(80 + variant_);

    timer_ -= delta;

    if (timer_ < 0) {
        if (auto room = source_->get_room(origin_tile_)) {
            room->___rewind___ability_used(pfrm, app);
        } else if (auto drone = source_->get_drone(origin_tile_)) {
            (*drone)->___rewind___ability_used(pfrm, app);
        }
        kill();
    }
}



extern Sound sound_impact;



void NemesisBlast::on_collision(Platform& pfrm, App& app, Room& room)
{
    if (source_ == room.parent()) {
        if (room.position().x == origin_tile_.x or
            room.position().x + 1 == origin_tile_.x or
            room.position().x + (room.size().x - 1) == origin_tile_.x) {
            // Because we do not want to include collisions with the originating
            // cannon, or with any blocks directly above or below the cannon.
            return;
        }
        if (auto origin = source_->get_room(origin_tile_)) {
            if (origin == &room) {
                return;
            }
        }
    }

    if (source_ == room.parent() and is_forcefield(room.metaclass())) {
        return;
    }

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < damage()) {
        room.apply_damage(pfrm, app, Room::health_upper_limit());
        return;
    }


    timestream_record_destroyed(pfrm, app);

    room.apply_damage(pfrm, app, damage(), source_);

    if (str_eq(room.name(), "mirror-hull")) {
        room.set_ai_aware(pfrm, app, true);
        timestream_record_destroyed(pfrm, app);
        step_vector_.x *= -1;
        step_vector_.y *= -1;
        source_ = room.parent();
        origin_tile_ = room.position();
        timer_ = 0;
        pfrm.speaker().play_sound("cling", 2);
    } else {
        kill();
        app.camera()->shake(2 + variant_ * 6);
        if (variant_ < 2) {
            medium_explosion(pfrm, app, sprite_.get_position());
        } else {
            big_explosion(pfrm, app, sprite_.get_position());
        }
        if (room.health()) {
            sound_impact.play(pfrm, 1);
        }
    }
}



void NemesisBlast::on_collision(Platform& pfrm, App& app, Entity& entity)
{
    timestream_record_destroyed(pfrm, app);

    kill();
    app.camera()->shake(2 + variant_ * 6);
    medium_explosion(pfrm, app, sprite_.get_position());

    entity.apply_damage(pfrm, app, damage());
}



Health NemesisBlast::damage() const
{
    switch (variant_) {
    default:
    case 0:
        return nemesis_blast_damage;

    case 1:
        return nemesis_blast_damage * 2;

    case 2:
        return nemesis_blast_damage * 4;
    }
}



void NemesisBlast::timestream_record_destroyed(Platform& pfrm, App& app)
{
    auto timestream_record = [&](time_stream::event::NemesisBlastDestroyed& c) {
        c.x_origin_ = origin_tile_.x;
        c.y_origin_ = origin_tile_.y;
        c.timer_.set(timer_);
        c.x_pos_.set(sprite_.get_position().x.as_integer());
        c.y_pos_.set(sprite_.get_position().y.as_integer());
        c.x_speed__data_.set(step_vector_.x.data());
        c.y_speed__data_.set(step_vector_.y.data());
    };


    if (source_ == &app.player_island()) {
        time_stream::event::PlayerNemesisBlastDestroyed e;
        timestream_record(e);
        e.variant_ = variant_;
        app.time_stream().push(app.level_timer(), e);
    } else {
        time_stream::event::PlayerNemesisBlastDestroyed e;
        timestream_record(e);
        e.variant_ = variant_;
        app.time_stream().push(app.level_timer(), e);
    }
}



} // namespace skyland