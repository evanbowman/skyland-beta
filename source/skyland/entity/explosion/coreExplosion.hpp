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

#include "explosion.hpp"
#include "explosion2.hpp"
#include "skyland/entity.hpp"
#include "skyland/skyland.hpp"
#include "skyland/timeStreamEvent.hpp"



namespace skyland
{



class CoreExplosionQuarter : public Entity
{
public:
    CoreExplosionQuarter(Platform::DynamicTexturePtr dt,
                         const Vec2<Fixnum>& pos,
                         int quarter)
        : Entity({{}, {}}), dt_(dt), timer_(0), quarter_(quarter)
    {
        sprite_.set_texture_index(dt->mapping_index());

        auto p = pos;

        switch (quarter) {
        case 1:
            sprite_.set_flip({true, false});
            p.x += 32.0_fixed;
            break;

        case 2:
            sprite_.set_flip({false, true});
            p.y += 32.0_fixed;
            break;

        case 3:
            sprite_.set_flip({true, true});
            p.x += 32.0_fixed;
            p.y += 32.0_fixed;
            break;
        }

        sprite_.set_position(p);

        dt_->remap(70 * 2);
    }


    void update(Time delta) override
    {
        timer_ += delta * 2;

        if (timer_ > milliseconds(200)) {
            timer_ -= milliseconds(200);


            if (keyframe_ == 5) {
                kill();
                if (quarter_ == 0) {
                    time_stream::event::CoreExplosion e;
                    const auto pos = sprite_.get_position();
                    e.x_.set(pos.x.as_integer());
                    e.y_.set(pos.y.as_integer());
                    APP.time_stream().push(APP.level_timer(), e);
                }
            } else {
                if (quarter_ == 0) {
                    dt_->remap((70 + keyframe_) * 2);
                }
                keyframe_++;
            }
        }
    }


    void jump_to_end()
    {
        timer_ = milliseconds(200);
        keyframe_ = 4;
        dt_->remap((70 + keyframe_) * 2);
    }


    void rewind(Time delta) override
    {
        timer_ -= delta * 2;

        if (timer_ < 0) {
            timer_ += milliseconds(200);

            if (keyframe_ == 0) {
                kill();
            } else {
                if (quarter_ == 0) {
                    dt_->remap((70 + keyframe_) * 2);
                }
                keyframe_--;
            }
        }
    }


private:
    Platform::DynamicTexturePtr dt_;
    Time timer_;
    int quarter_;
    int keyframe_ = 0;
};



struct CoreExplosionConfig
{
    bool clear_effects_ = true;
    bool ripple_effect_ = true;
    u8 screenshake_ = 28;
    int arms_ = 4;
    int rot_ = 0;
};



inline void core_explosion(Island* parent,
                           const Vec2<Fixnum>& pos,
                           const CoreExplosionConfig& conf = {})
{
    if (parent->core_count() == 1) {
        // There's a special death sequence animation for the final destroyed
        // core.
        big_explosion(pos);
        return;
    }


    int min_x = PLATFORM.screen().get_view().int_center().x - 48;
    int max_x = PLATFORM.screen().get_view().int_center().x +
                PLATFORM.screen().size().x + 48;
    int max_y = 700;
    int min_y = 450;

    if (pos.y.as_integer() > max_y or pos.y.as_integer() < min_y or
        pos.x.as_integer() > max_x or pos.x.as_integer() < min_x) {
        // Don't create the explosion effect if way outside of the camera range.
    } else {

        if (conf.clear_effects_) {
            APP.clear_effects_lowpriority();
        }

        int pitch = 360 / conf.arms_;

        for (int i = 0; i < 8; ++i) {
            for (int j = 0; j < conf.arms_; ++j) {
                const int angle = j * pitch + 45 + i * 3 + conf.rot_;
                const u8 half_angle = angle / 2;
                if (auto exp =
                        APP.alloc_entity<Explosion2>(pos, half_angle, (u8)i)) {
                    auto dir = rotate({1, 0}, angle);
                    dir = dir * (((i + 1 / 2.f) * 1.5f) * 0.00005f);
                    Vec2<Fixnum> spd;
                    spd.x = Fixnum(dir.x);
                    spd.y = Fixnum(dir.y);
                    exp->set_speed(spd);
                    APP.effects().push(std::move(exp));
                }
            }
        }
    }

    if (conf.ripple_effect_) {
        auto dt = PLATFORM.make_dynamic_texture();
        if (dt) {
            auto p = pos;
            p.x -= 32.0_fixed;
            p.y -= 32.0_fixed;
            auto make_segment = [&](int q) {
                return APP.effects().push(
                    APP.alloc_entity<CoreExplosionQuarter>(*dt, p, q));
            };
            make_segment(3);
            make_segment(2);
            make_segment(1);
            make_segment(0);
        }
    }

    APP.camera()->shake(conf.screenshake_);
}



} // namespace skyland
