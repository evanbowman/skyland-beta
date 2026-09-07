////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "windmill.hpp"
#include "skyland/latency.hpp"
#include "skyland/room_metatable.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



Power Windmill::power_usage() const
{
    const auto base_power = (*metaclass())->consumes_power();
    auto power = base_power;

    if (APP.environment().is_overcast()) {
        power *= 2;
    }

    return power;
}



void Windmill::collect_sprites(Buffer<Sprite, 4, false>& out) const
{
    auto pos = visual_center();
    pos.x -= 8.0_fixed;
    pos.y -= 16.0_fixed;

    Sprite sail;
    sail.set_size(Sprite::Size::w16_h32);
    sail.set_texture_index(120);

    const auto& cursor = parent()->layer() == Layer::map_0_ext
                             ? globals().near_cursor_loc_
                             : globals().far_cursor_loc_;

    if (cursor == position()) {

    } else if (parent()->rooms_view().get(cursor.x, cursor.y) and
               parent()->interior_visible() and
               abs(cursor.x - position().x) < 3 and
               abs(cursor.y - position().y) < 3) {

        sail.set_alpha(Sprite::Alpha::translucent);
        sail.set_mix({custom_color(0x103163), 128});
        sail.set_priority(2);
    }



    const auto rot1 = rot_.as_integer();
    auto rot2 = rot1 + 90;
    auto rot3 = rot2 + 90;
    auto rot4 = rot3 + 90;

    auto show = [&](int rot) {
        auto spos = pos;
        auto rv = rotation(rot);
        spos.x += rv.x * 16.0_fixed;
        spos.y += rv.y * 16.0_fixed;
        sail.set_position(spos);
        int sail_rot = rot + 90;
        if (sail_rot > 359) {
            sail_rot -= 359;
        }
        sail.set_rotation(-(INT16_MAX / 360) * sail_rot);
        out.push_back(sail);
    };

    show(rot1);
    show(rot2);
    show(rot3);
    show(rot4);
}



void Windmill::display(Platform::Screen& screen)
{
    Buffer<Sprite, 4, false> buf;
    collect_sprites(buf);

    for (auto& spr : buf) {
        if (screen.fade_active()) {
            spr.set_mix({});
        }
        screen.draw(spr);
    }
}



class SailDebris : public Entity
{
public:
    SailDebris(const Sprite& spr, int max_y) : Entity({}), max_y_(max_y)
    {
        sprite_ = spr;
    }


    void update(Time delta) override
    {
        // NOTE: a lot of our update logic simply multiplies speed by delta
        // time. But debris has gravity applied, so we run multiple update steps
        // in a loop.

        if (delta == 0) {
            return;
        }
        timer_ += delta;
        if (timer_ > seconds(2)) {
            kill();
        }

        delta += remainder_;
        remainder_ = 0;

        auto pos = sprite_.get_position();
        while (delta >= 16666 / 4) {
            delta -= 16666 / 4;
            speed_.y = speed_.y + gravity_;
            pos = pos + speed_;
        }
        remainder_ += delta;
        sprite_.set_position(pos);

        if (pos.y > max_y_) {
            kill();
        }
    }


    void rewind(Time delta) override
    {
        kill();
    }


    Vec2<Fixnum> speed_;
    Fixnum gravity_;

private:
    Time timer_ = 0;
    Time remainder_ = 0;
    Fixnum max_y_;
};



void Windmill::finalize()
{
    Room::finalize();

    const auto max_y =
        parent()->origin().y + 16.0_fixed * 16.0_fixed + 32.0_fixed;

    if (health() == 0) {
        Buffer<Sprite, 4, false> sails;
        collect_sprites(sails);

        for (const auto& spr : sails) {
            if (auto e = alloc_entity<SailDebris>(spr, max_y.as_integer())) {
                int angle = rng::choice<45>(rng::utility_state);
                if (rng::choice<2>(rng::utility_state)) {
                    angle = 360 - 45;
                }
                auto dir = rotate({0, 1}, angle);
                e->speed_.x = Fixnum(dir.x);
                e->speed_.y = Fixnum(-1 * dir.y);
                e->speed_.y -= 0.3_fixed;
                e->speed_ = e->speed_ * 0.5_fixed;
                e->gravity_ =
                    Fixnum(0.04f * 0.15f) +
                    Fixnum::from_integer(rng::choice<3>(rng::utility_state)) *
                        0.003_fixed;
                APP.effects().push(std::move(e));
            }
        }
    }
}



void Windmill::update(Time delta)
{
    Room::update(delta);
    Room::ready();

    // FIXME: bad hack. Rooms are appended to an unordered priority list and
    // drawn in arbitrary order. Because windmill blades overlap with crewmember
    // sprites, the sprites flicker due to differing priority on each frame. I
    // need a more robust solution...
    parent()->drawfirst(this);

    auto rate = 0.00005_fixed;
    if (APP.environment().is_overcast()) {
        rate *= 2.0_fixed;
    }

    rot_ += rate * APP.delta_fp();
    while (rot_ > 359.0_fixed) {
        rot_ -= 359.0_fixed;
    }

    if (dup_check_ >= (int)player_island().rooms().size()) {
        dup_check_ = 0;
    } else if (is_player_island(parent())) {
        auto& room = *player_island().rooms()[dup_check_++];
        if (&room not_eq this and room.metaclass() == this->metaclass()) {
            // Only one windmill allowed per island. Because we only allow so
            // many sprites per scanline.
            room.apply_damage(Room::health_upper_limit());
        }
    }
}



void Windmill::rewind(Time delta)
{
    Room::rewind(delta);

    auto rate = 0.00005_fixed;
    if (APP.environment().is_overcast()) {
        rate *= 2.0_fixed;
    }

    rot_ -= rate * APP.delta_fp();
    while (rot_ < 0.0_fixed) {
        rot_ += 359.0_fixed;
    }
}



} // namespace skyland
