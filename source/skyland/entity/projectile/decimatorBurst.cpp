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


#include "decimatorBurst.hpp"
#include "skyland/entity/drones/drone.hpp"
#include "skyland/entity/explosion/explosion.hpp"
#include "skyland/room.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/rooms/cannon.hpp"
#include "skyland/rooms/decimator.hpp"
#include "skyland/rooms/forcefield.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



static SHARED_VARIABLE(decimator_burst_damage);



DecimatorBurst::DecimatorBurst(const Vec2<Fixnum>& position,
                               const Vec2<Fixnum>& target,
                               Island* source,
                               const RoomCoord& origin_tile)
    : Projectile({{10, 20}, {8, 16}}), source_(source),
      origin_tile_(origin_tile)
{
    sprite_.set_position(position);
    sprite_.set_size(Sprite::Size::w16_h32);
    sprite_.set_texture_index(72);

    sprite_.set_origin({8, 16});

    static const Float speed = 0.00025f;
    const auto step = direction(fvec(position), fvec(target)) * speed;
    step_vector_ = Vec2<Fixnum>{Fixnum(step.x), Fixnum(step.y)};
}



void DecimatorBurst::update(Time delta)
{
    auto pos = sprite_.get_position();
    pos = pos + APP.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    if (source_ not_eq &APP.player_island()) {
        sprite_.set_flip({true, false});
    }

    timer_ += delta;

    Island* target;
    if (is_player_island(source_)) {
        target = APP.opponent_island();
    } else {
        target = &APP.player_island();
    }

    if (target) {
        int max_x = 9999999;
        int min_x = -9999999;
        if (is_player_island(target)) {
            // If we're shooting at the player's island, the projectile moves
            // leftwards, and we care about the min bound.
            min_x = target->origin().x.as_integer() - 32;
        } else {
            // Otherwise, we need to check the max bound.
            max_x = target->origin().x.as_integer() +
                    16 * target->terrain().size() + 32;
        }
        if (pos.x.as_integer() > max_x or pos.x.as_integer() < min_x) {
            kill();
        }
    }


    if (timer_ > milliseconds(1500)) {
        kill();
    }
}



void DecimatorBurst::rewind(Time delta)
{
    auto pos = sprite_.get_position();
    pos = pos - APP.delta_fp() * step_vector_;
    sprite_.set_position(pos);

    timer_ -= delta;

    if (source_ not_eq &APP.player_island()) {
        sprite_.set_flip({true, false});
    }

    if (auto r = source_->get_room(origin_tile_)) {
        if (auto dec = r->cast<Decimator>()) {
            if (dec->counter_ < burst_index_) {
                kill();
            }
        }
    }

    if (timer_ <= 0) {
        kill();
    }
}



void DecimatorBurst::on_collision(Room& room, Vec2<u8> origin)
{
    if (source_ == room.parent()) {
        if (room.position().x == origin_tile_.x or
            room.position().x + 1 == origin_tile_.x) {
            // Because we do not want to include collisions with the originating
            // cannon, or with any blocks directly above or below the cannon.
            return;
        }
    }

    if ((*room.metaclass())->properties() & RoomProperties::fragile and
        room.max_health() < decimator_burst_damage) {
        room.apply_damage(Room::health_upper_limit());
        return;
    }

    if (source_ == room.parent() and is_forcefield(room.metaclass())) {
        return;
    }


    auto timestream_record =
        [&](time_stream::event::BasicProjectileDestroyed& e) {
            e.x_origin_ = origin_tile_.x;
            e.y_origin_ = origin_tile_.y;
            e.timer_.set(timer_);
            e.x_pos_.set(sprite_.get_position().x.as_integer());
            e.y_pos_.set(sprite_.get_position().y.as_integer());
            e.x_speed__data_.set(step_vector_.x.data());
            e.y_speed__data_.set(step_vector_.y.data());
        };


    if (is_player_island(source_)) {
        time_stream::event::PlayerDecimatorBurstDestroyed e;
        timestream_record(e);
        APP.time_stream().push(APP.level_timer(), e);
    } else {
        time_stream::event::OpponentDecimatorBurstDestroyed e;
        timestream_record(e);
        APP.time_stream().push(APP.level_timer(), e);
    }


    kill();
    APP.camera()->shake(26);
    big_explosion(sprite_.get_position(),
                  BigExplosionConfig{.centerflash_ = true});

    room.apply_damage(decimator_burst_damage);
}



void DecimatorBurst::on_collision(Entity& entity)
{
    // Blows through drones, does not stop.

    APP.camera()->shake(4);

    entity.apply_damage(decimator_burst_damage);
}



} // namespace skyland
