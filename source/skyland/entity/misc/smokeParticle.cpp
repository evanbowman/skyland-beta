#include "smokeParticle.hpp"
#include "skyland/skyland.hpp"
#include "number/random.hpp"



namespace skyland
{



SmokeParticle::SmokeParticle(const Vec2<Fixnum>& pos) :
    AnimatedEffect(pos, 0, 0, milliseconds(80))
{
    sprite_.set_size(Sprite::Size::w8_h8);
    begin_tile_ = 8 * 93;
    end_tile_ = 8 * 93 + 4;
    sprite_.set_texture_index(begin_tile_);
    sprite_.set_origin({4, 4});
    bool x_flip = rng::choice<2>(rng::utility_state);
    bool y_flip = rng::choice<2>(rng::utility_state);
    sprite_.set_flip({x_flip, y_flip});
}



void SmokeParticle::update(Time delta)
{
    if (ignore_gamespeed_) {
        delta = PLATFORM.delta_clock().last_delta();
    }

    AnimatedEffect::update(delta);

    auto pos = sprite_.get_position();
    pos = pos + Fixnum::from_integer(delta) * velocity_;
    sprite_.set_position(pos);
}



void SmokeParticle::rewind(Time delta)
{
    AnimatedEffect::rewind(delta);

    auto pos = sprite_.get_position();
    pos = pos - APP.delta_fp() * velocity_;
    sprite_.set_position(pos);
}



void SmokeParticle::move(Angle dir, Fixnum speed)
{
    velocity_ = make_velocity(dir, speed);
}



void SmokeParticle::set_color(ColorConstant c)
{
    sprite_.set_mix({c, 255});
}



void SmokeParticle::spawn(const Vec2<Fixnum>& pos,
                          int n,
                          Optional<ColorConstant> color)
{
    for (int i = 0; i < n; ++i) {
        if (auto sp = alloc_entity<SmokeParticle>(pos)) {
            auto angle = rng::choice<360>(rng::utility_state);
            auto speed = 0.00003_fixed;
            if (rng::choice<2>(rng::utility_state)) {
                speed += 0.0000025_fixed;
            } else {
                speed -= 0.0000025_fixed;
            }
            sp->move(angle, speed);
            if (color) {
                sp->set_color(*color);
            }
            APP.effects().push(std::move(sp));
        }
    }
}



}
