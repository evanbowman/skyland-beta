#pragma once

#include "platform/platform.hpp"
#include "skyland/scene.hpp"
#include "skyland/entity.hpp"
#include "skyland/skyland.hpp"
#include "modules/dataCartModule.hpp"



namespace skyland
{



void init_clouds();



namespace
{



#define INT16_BITS (8 * sizeof(int16_t))
#ifndef INT16_MAX
#define INT16_MAX ((1 << (INT16_BITS - 1)) - 1)
#endif

#define TABLE_BITS (5)
#define TABLE_SIZE (1 << TABLE_BITS)
#define TABLE_MASK (TABLE_SIZE - 1)

#define LOOKUP_BITS (TABLE_BITS + 2)
#define LOOKUP_MASK ((1 << LOOKUP_BITS) - 1)
#define FLIP_BIT (1 << TABLE_BITS)
#define NEGATE_BIT (1 << (TABLE_BITS + 1))
#define INTERP_BITS (INT16_BITS - 1 - LOOKUP_BITS)
#define INTERP_MASK ((1 << INTERP_BITS) - 1)


static constexpr const int16_t sin90[TABLE_SIZE + 1] = {
    0x0000, 0x0647, 0x0c8b, 0x12c7, 0x18f8, 0x1f19, 0x2527, 0x2b1e, 0x30fb,
    0x36b9, 0x3c56, 0x41cd, 0x471c, 0x4c3f, 0x5133, 0x55f4, 0x5a81, 0x5ed6,
    0x62f1, 0x66ce, 0x6a6c, 0x6dc9, 0x70e1, 0x73b5, 0x7640, 0x7883, 0x7a7c,
    0x7c29, 0x7d89, 0x7e9c, 0x7f61, 0x7fd7, 0x7fff};



constexpr inline s16 sine(s16 angle)
{
    s16 v0 = 0;
    s16 v1 = 0;
    if (angle < 0) {
        angle += INT16_MAX;
        angle += 1;
    }
    v0 = (angle >> INTERP_BITS);
    if (v0 & FLIP_BIT) {
        v0 = ~v0;
        v1 = ~angle;
    } else {
        v1 = angle;
    }
    v0 &= TABLE_MASK;
    v1 = sin90[v0] +
         (s16)(((int32_t)(sin90[v0 + 1] - sin90[v0]) * (v1 & INTERP_MASK)) >>
               INTERP_BITS);
    if ((angle >> INTERP_BITS) & NEGATE_BIT)
        v1 = -v1;
    return v1;
}


constexpr inline s16 cosine(s16 angle)
{
    if (angle < 0) {
        angle += INT16_MAX;
        angle += 1;
    }
    return sine(angle - s16((270.f / 360.f) * INT16_MAX));
}



namespace detail
{
template <std::size_t... Is> struct seq
{
};
template <std::size_t N, std::size_t... Is>
struct gen_seq : gen_seq<N - 1, N - 1, Is...>
{
};
template <std::size_t... Is> struct gen_seq<0, Is...> : seq<Is...>
{
};



constexpr inline Vec2<Float> rotv(const Vec2<Float>& input, Float angle)
{
    const s16 converted_angle = INT16_MAX * (angle / 360.f);
    const Float cos_theta = Float(cosine(converted_angle)) / INT16_MAX;
    const Float sin_theta = Float(sine(converted_angle)) / INT16_MAX;

    return {input.x * cos_theta - input.y * sin_theta,
            input.x * sin_theta + input.y * cos_theta};
}



template <class Generator, std::size_t... Is>
constexpr auto generate_array_helper(Generator g, seq<Is...>)
    -> std::array<decltype(g(std::size_t{}, sizeof...(Is))), sizeof...(Is)>
{
    return {{g(Is, sizeof...(Is))...}};
}



template <std::size_t tcount, class Generator>
constexpr auto generate_array(Generator g)
    -> decltype(generate_array_helper(g, gen_seq<tcount>{}))
{
    return generate_array_helper(g, gen_seq<tcount>{});
}
} // namespace detail



inline constexpr auto make_rotation_lut(float v)
{
    return detail::generate_array<360>(
        [](std::size_t curr, std::size_t total) -> Vec2<Fixnum> {
            auto off = detail::rotv({1.f, 0}, curr);
            return Vec2<Fixnum>{Fixnum(off.x), Fixnum(off.y)};
        });
}

}



static constexpr const auto rotation_lut = make_rotation_lut(0.f);


static Fixnum whole(Fixnum v)
{
    return Fixnum::from_integer(
                                (v + (v < 0.0_fixed ? Fixnum::from_integer(-1) * 0.5_fixed : 0.5_fixed)).as_integer());
}


class SailingSimulatorScene : public Scene
{
public:


    class WakeRipple : public Entity
    {
    public:
        WakeRipple(Vec2<Fixnum> pos) : Entity({})
        {
            sprite_.set_size(Sprite::Size::w8_h8);
            sprite_.set_tidx_8x8(30, 0);
            sprite_.set_position(pos);
            sprite_.set_origin({});
        }


        void update(Time delta) override
        {
            // The game manipulates the time delta for slow motion stuff, etc. But
            // we always want this UI effect to play at the same rate.
            delta = PLATFORM.delta_clock().last_delta();

            timer_ += delta / 2;
            if (timer_ >= milliseconds(80)) {
                timer_ -= milliseconds(80);
                auto t = sprite_.get_texture_index();
                if (t == 30 * 8 + 5) {
                    kill();
                    return;
                }

                ++t;
                sprite_.set_texture_index(t);
            }
        }


        void rewind(Time delta) override
        {
            kill();
        }


        Sprite& sprite()
        {
            return sprite_;
        }

    private:
        Time timer_ = 0;
    };



    static void make_wake_effect(Vec2<Fixnum> pos)
    {
        auto segment = [&](Fixnum xoff, Fixnum yoff, bool xflip, bool yflip) {
            auto p = pos;
            p.x += xoff;
            p.y += yoff;
            if (auto e = APP.alloc_entity<WakeRipple>(p)) {
                e->sprite().set_flip({xflip, yflip});
                APP.effects().push(std::move(e));
            }
        };

        segment(Fixnum::from_integer(-4), Fixnum::from_integer(-4), false, false);
    }




    using Wind = Fixnum; // angle out of 360


    class Boat
    {
    private:
        EntityList<Entity> projectiles_;
        Optional<Platform::DynamicTexturePtr> sail_texture_1_;
        Optional<Platform::DynamicTexturePtr> sail_texture_2_;
        Vec2<Fixnum> position_;
        Vec2<Fixnum> drift_ = {}; // NOTE: recoil, impacts, knockback, etc.
        Fixnum heel_;
        Fixnum rotation_;
        Fixnum sail_force_;
        Fixnum boom_angle_ = 0.0_fixed;
        bool   boom_ready_ = false;
        bool   last_port_    = false;
        bool   jibing_       = false;
        bool depowered_      = false;
        u16 hardware_rotation_ = 0;
        u8 wake_timer_;
        u8 mix_amt_ = 128;

    public:


        static constexpr const int no_go = 35;


        void apply_impulse(const Vec2<Fixnum>& impulse)
        {
            drift_ = drift_ + impulse;
        }


        Vec2<Fixnum> velocity() const
        {
            return sail_force_vector() + drift_;
        }


        int relative_wind_angle(Fixnum wind_from_deg) const
        {
            int diff = rotation_.as_integer() - wind_from_deg.as_integer();
            diff %= 360;
            if (diff < 0)   diff += 360;
            if (diff > 180) diff = 360 - diff;   // fold: port and starboard are symmetric
            return diff;
        }


        Fixnum sail_efficiency(Fixnum wind_from_deg) const
        {
            const int rel = relative_wind_angle(wind_from_deg);

            if (rel < no_go) {
                return 0.0_fixed;             // sails luffing, no drive
            }

            // sin(rel): ~0.71 close-hauled, 1.0 on the beam, back toward 0 at a run.
            Fixnum hump = rotation_lut[rel].y;

            if (rel <= 90) {
                const Fixnum upwind_floor = 0.7_fixed;
                if (hump < upwind_floor) hump = upwind_floor;
            }

            if (rel > 90) {
                // A boat still runs downwind, just slower than it reaches -- don't let
                // a dead run collapse to zero.
                const Fixnum downwind_floor = 0.8_fixed;
                if (hump < downwind_floor) hump = downwind_floor;
            }

            return hump;
        }


        const char* fmt_point_of_sail(Fixnum wind_from_deg) const
        {
            const int rel = relative_wind_angle(wind_from_deg);

            if (depowered_) {
                return "depowered";
            }

            if (rel < no_go) {
                const bool have_way = sail_force_ > 0.3_fixed;   // steerageway threshold
                return (have_way) ? "tacking" : "in irons";
            }

            if (rel < 60)  return "close hauled";
            if (rel < 80)  return "close reach";
            if (rel < 100) return "beam reach";
            if (rel < 160) return "broad reach";
            return "running";
        }


        const Vec2<Fixnum>& get_position()
        {
            return position_;
        }


        Boat() :
            sail_texture_1_(PLATFORM.make_dynamic_texture()),
            sail_texture_2_(PLATFORM.make_dynamic_texture())
        {
            position_ = {120.0_fixed, 80.0_fixed};
        }


        Vec2<Fixnum> sail_force_vector() const
        {
            return {
                sail_force_ * rotation_lut[rotation_.as_integer()].x,
                sail_force_ * rotation_lut[rotation_.as_integer()].y
            };
        }


        void update(const Wind& wind)
        {
            // --- Helm: full rudder once there's way on; fades only near stall --------
            static const Fixnum STEERAGE = 0.4_fixed;   // speed at which the rudder reaches full authority
            Fixnum mag  = sail_force_ < 0.0_fixed ? (0.0_fixed - sail_force_) : sail_force_;
            Fixnum auth = mag / STEERAGE;               // full authority once speed >= STEERAGE
            if (auth > 1.0_fixed) auth = 1.0_fixed;

            Fixnum turn = 2.0_fixed * auth;
            if (sail_force_ < 0.0_fixed) turn = 0.0_fixed - turn;   // sternway reverses the helm

            auto apply_turn = [&](Fixnum d) {
                rotation_ += d;
                while (rotation_ <  0.0_fixed)   rotation_ += 360.0_fixed;
                while (rotation_ >= 360.0_fixed) rotation_ -= 360.0_fixed;
            };

            if (PLATFORM.input().pressed<Button::left>()) {
                apply_turn(0.0_fixed - turn);
            } else if (PLATFORM.input().pressed<Button::right>()) {
                apply_turn(turn);
            }

            update_entities(milliseconds(17), projectiles_);

            if (button_down<Button::alt_1>()) {
                depowered_ = not depowered_;
            }

            hardware_rotation_ = -1 * ((rotation_ * 0.002777_fixed) * Fixnum::from_integer(65535 / 2)).as_integer() + 65535 / 8;


            static const auto wind_strength = 1.75_fixed;

            Fixnum target;

            if (depowered_) {
                target = 0.0_fixed;
            } else {
                const Fixnum drive = sail_efficiency(wind) * wind_strength;
                if (drive > 0.0_fixed) {
                    target = drive;                        // sails drawing: normal drive
                } else {
                    // No-go zone: sails luff. Head to wind, the rig pushes us backward.
                    // Strongest dead-upwind (rel==0), fading to zero at the no-go edge.
                    const int rel  = relative_wind_angle(wind);            // 0 == dead upwind
                    Fixnum     frac = Fixnum::from_integer(no_go - rel) * 0.0285_fixed; // /35
                    target = 0.0_fixed - (0.35_fixed * frac);              // small negative
                }
            }

            const Fixnum K = 0.01_fixed;
            sail_force_ += (target - sail_force_) * K;

            position_ = position_ + sail_force_vector() + drift_;
            // Bleed off impulses. The sail term above is untouched, so with no
            // impulses drift_ stays {} and behavior is byte-for-byte the current model.
            const Fixnum drag = 0.90_fixed;
            const Fixnum eps  = 0.05_fixed;
            drift_ = drag * drift_;
            if (drift_.x < eps && drift_.x > eps * Fixnum::from_integer(-1) &&
                drift_.y < eps && drift_.y > eps * Fixnum::from_integer(-1)) {
                drift_ = {}; // snap to zero
            }

            Fixnum boom_target;
            bool   port_now    = on_port_tack(wind);

            if (depowered_) {
                const int rel = relative_wind_angle(wind);
                const int mag = (rel <= 90) ? rel : (180 - rel);   // triangle wave
                boom_target = on_port_tack(wind) ? Fixnum::from_integer(-mag)   // same
                    : Fixnum::from_integer(mag);   // leeward sign
            } else {
                boom_target = boom_offset(wind);
            }

            if (!boom_ready_) {
                boom_angle_ = boom_target;
                last_port_  = port_now;
                boom_ready_ = true;
            } else {
                // Detect a tack->tack crossing while eased well out == a jibe.
                if (port_now != last_port_) {
                    bool far_out = (boom_angle_ > 60.0_fixed) || (boom_angle_ < Fixnum::from_integer(-1) * 60.0_fixed);
                    if (far_out) jibing_ = true;   // must sheet in before releasing
                }
                last_port_ = port_now;

                if (jibing_) {
                    // Phase 1: haul the boom to center, fast and controlled.
                    boom_angle_ += (0.0_fixed - boom_angle_) * 0.22_fixed;
                    if (boom_angle_ > Fixnum::from_integer(-1)*6.0_fixed && boom_angle_ < 6.0_fixed) {
                        jibing_ = false;           // crossed center; hand back to normal trim
                    }
                } else {
                    // Phase 2 (and all normal trimming): ease toward optimal.
                    boom_angle_ += (boom_target - boom_angle_) * 0.06_fixed;
                }
            }

            // if (heel_ < 0.75_fixed) {
            //     heel_ = heel_ + 0.01_fixed;
            // } else {
            //     heel_ = 0.0_fixed;
            // }

            wake_timer_++;
            if (wake_timer_ > 5) {
                wake_timer_ = 0;
                make_wake_effect(rng::sample<3>(get_position(),
                                                rng::utility_state));
            }
        }


        void display(Fixnum wind_from_deg)
        {
            draw_mast(wind_from_deg);

            for (auto& proj : projectiles_) {
                PLATFORM.screen().draw(proj->sprite());
            }

            for (int i = 4; i > -1; --i) {
                draw_slice(i);
            }
        }

        Vec2<Fixnum> center() const
        {
            auto pos = position_;
            pos.x -= 8.0_fixed;
            pos.y -= 16.0_fixed;
            return pos;
        }

        void draw_slice(int n)
        {
            Sprite spr;
            if (n < 2) {
                spr.set_priority(2);
            }
            spr.set_size(Sprite::Size::w16_h32);
            spr.set_texture_index(12 + n);
            auto pos = position_;
            pos.x -= 8.0_fixed;
            pos.y -= 16.0_fixed;
            pos.x -= heel_ * Fixnum::from_integer(n);
            pos.y -= Fixnum::from_integer(n * 2);
            spr.set_position(pos);
            spr.set_rotation(hardware_rotation_);
            PLATFORM.screen().draw(spr);
        }


        Fixnum boom_swing(Fixnum wind_from_deg) const
        {
            const int rel = relative_wind_angle(wind_from_deg);
            int swing = rel / 2;              // ~22 close-hauled, 45 beam, 90 run
            const int min_swing = 8;          // never dead-flat on the centerline; looks wrong
            if (swing < min_swing) swing = min_swing;
            return Fixnum::from_integer(swing);
        }


        bool on_port_tack(Fixnum wind_from_deg) const
        {
            int diff = (rotation_.as_integer() - wind_from_deg.as_integer()) % 360;
            if (diff < 0) diff += 360;
            return diff < 180;   // wind coming over one side vs. the other
        }


        Fixnum boom_offset(Fixnum wind_from_deg) const
        {
            Fixnum swing = boom_swing(wind_from_deg);
            return on_port_tack(wind_from_deg) ? (Fixnum::from_integer(-1) * swing) : swing;
        }


        static u16 deg_to_hw(Fixnum deg)
        {
            return -1 * ((deg * 0.002777_fixed) * Fixnum::from_integer(65535 / 2)).as_integer()
                + 65535 / 8;
        }


        static constexpr int HALF_BOOM   = 9;    // box center -> boom end
        static constexpr int MOUNT_CLEAR = 4;    // push aft to clear the mast pole; tune to taste
        static constexpr int MAST_FWD    = 10;
        static constexpr int GOOSE_RAISE = 13;   // 3 = mast foot, 40 = masthead

        void draw_sail(Fixnum wind_from_deg)
        {
            if (!sail_texture_1_ || !sail_texture_2_) return;

            // Same screen bearing the boom uses, folded into the baked -90..90 half.
            int b = (rotation_ + 180.0_fixed + boom_angle_).as_integer() % 360;
            if (b < 0)   b += 360;
            if (b > 180) b -= 360;
            bool flip = false;
            if (b > 90)       { b = 180 - b;  flip = true; }
            else if (b < -90) { b = -180 - b; flip = true; }
            int frame = (b + 90) / 2;
            if (frame < 0)  frame = 0;
            if (frame > 90) frame = 90;

            const int base_index = 64;
            const int base_block = base_index + frame * 2;   // block number of the upper cell
            const int upper = 2 * base_block;                // -> 16x32 tile units
            const int lower = upper + 2;                      // next 32x32 block = +2 tiles, not +1

            const int hd = rotation_.as_integer();
            Vec2<Fixnum> goose = position_;
            goose.x += whole(Fixnum::from_integer(MAST_FWD) * rotation_lut[hd].x);
            goose.y += whole(Fixnum::from_integer(MAST_FWD) * rotation_lut[hd].y);
            goose.y -= Fixnum::from_integer(GOOSE_RAISE + 1);

            const Fixnum tack_x = Fixnum::from_integer(flip ? 31 : 1);

            auto sail_color = custom_color(0xfceac2);
            u8 mix_amount = 255;

            const int rel = relative_wind_angle(wind_from_deg);
            if (rel < no_go or depowered_) {
                sail_color = ColorConstant::spanish_crimson;
                if (mix_amt_ < 255) {
                    mix_amt_ = clamp(mix_amt_ + 16, 0, 255);
                    mix_amount = mix_amt_;
                }
            } else {
                mix_amt_ = 128;
            }

            // LOWER block: its row 0 at tack_x IS the tack -> land that on the gooseneck.
            (*sail_texture_1_)->remap(lower);
            Sprite lo;
            lo.set_mix({sail_color, mix_amount});
            lo.set_size(Sprite::Size::w32_h32);
            lo.set_flip({flip, 0});
            lo.set_texture_index((*sail_texture_1_)->mapping_index());
            Vec2<Fixnum> lo_pos = goose;
            lo_pos.x -= tack_x;
            lo.set_position(lo_pos);
            PLATFORM.screen().draw(lo);

            (*sail_texture_2_)->remap(upper);
            Sprite hi;
            hi.set_mix({sail_color, mix_amount});
            hi.set_size(Sprite::Size::w32_h32);
            hi.set_flip({flip, 0});
            hi.set_texture_index((*sail_texture_2_)->mapping_index());
            Vec2<Fixnum> hi_pos = lo_pos;
            hi_pos.y -= 32.0_fixed;
            hi.set_position(hi_pos);
            PLATFORM.screen().draw(hi);
        }

        void draw_boom()
        {
            Fixnum a = rotation_ + 180.0_fixed + boom_angle_;   // animated, not boom_offset()
            int ai = a.as_integer() % 360;
            if (ai < 0) ai += 360;

            const int hd = rotation_.as_integer();          // heading, for the gooseneck

            // LUT #1: deck center -> gooseneck, now riding up the mast
            Vec2<Fixnum> goose = position_;
            goose.x += whole(Fixnum::from_integer(MAST_FWD) * rotation_lut[hd].x);
            goose.y += whole(Fixnum::from_integer(MAST_FWD) * rotation_lut[hd].y);
            goose.y -= Fixnum::from_integer(GOOSE_RAISE);

            const int reach = HALF_BOOM + MOUNT_CLEAR;
            Vec2<Fixnum> center = goose;
            center.x += whole(Fixnum::from_integer(reach) * rotation_lut[ai].x);
            center.y += whole(Fixnum::from_integer(reach) * rotation_lut[ai].y);

            Sprite spr;
            spr.set_size(Sprite::Size::w16_h32);
            spr.set_texture_index(18);

            auto pos = center;
            pos.x -= 8.0_fixed;    // box center -> top-left (half of 16)
            pos.y -= 16.0_fixed;   // box center -> top-left (half of 32)
            spr.set_position(pos);

            spr.set_rotation(deg_to_hw(Fixnum::from_integer(ai)));
            PLATFORM.screen().draw(spr);
        }


        void draw_mast(Fixnum wind_from_deg)
        {
            draw_sail(wind_from_deg);
            draw_boom();

            Sprite spr;
            spr.set_size(Sprite::Size::w16_h32);
            spr.set_texture_index(17);
            auto pos = position_;
            pos.x -= 8.0_fixed;
            pos.y -= 16.0_fixed;
            pos.y -= 24.0_fixed;
            pos.x += whole(10.0_fixed * rotation_lut[rotation_.as_integer()].x);
            pos.y += whole(10.0_fixed * rotation_lut[rotation_.as_integer()].y);
            spr.set_rotation((Fixnum::from_integer(2100) * heel_).as_integer());
            spr.set_position(pos);
            PLATFORM.screen().draw(spr);
        }
    };


    struct State
    {
        Boat boat_;
        Wind wind_;
        Time anim_in_time_ = 0;
        int circ_radius_ = 0;
        bool exit_ = false;
    };


    SailingSimulatorScene() : state_(allocate<State>("sailing-state"))
    {
    }


    static constexpr const auto north_wind = 270.0_fixed;


    State& state()
    {
        return **state_;
    }


    void enter(Scene& prev) override
    {
        PLATFORM_EXTENSION(enable_parallax_clouds, false);

        for (int i = 0; i < 32; ++i) {
            for (int j = 0; j < 32; ++j) {
                PLATFORM.set_tile(Layer::background, i, j, 4);
            }
        }

        PLATFORM.load_sprite_texture("spritesheet_sailboat");
        PLATFORM.load_background_texture("background_sailing");
        PLATFORM.screen().schedule_fade(0);
        PLATFORM.fill_overlay(0);
        PLATFORM.clear_layer(Layer::map_0);
        PLATFORM.clear_layer(Layer::map_1);
        globals().entity_pools_.create("entity-mem");
        state().wind_ = north_wind;

        PLATFORM.speaker().stream_music("high_wind.raw", 0);
    }


    void exit(Scene& next) override
    {
        PLATFORM.load_sprite_texture("spritesheet");
        PLATFORM.load_background_texture("background");
        PLATFORM_EXTENSION(enable_parallax_clouds, true);
        init_clouds();
        APP.effects().clear();
        PLATFORM.speaker().stream_music("unaccompanied_wind.raw", 0);
    }


    ScenePtr update(Time delta) override
    {
        state().anim_in_time_ += delta;

        constexpr auto fade_duration = milliseconds(800);
        if (state().anim_in_time_ >= fade_duration) {
            state().circ_radius_ = -1;
        } else {
            auto amount = 1.f - smoothstep(0.f, fade_duration, state().anim_in_time_);
            state().circ_radius_ = 144 - int(144 * amount);
            if (state().anim_in_time_ > delta) {
                amount *= 0.75f;
            }
            // PLATFORM.screen().schedule_fade(amount);
        }

        update_entities(milliseconds(17), APP.effects());

        state().boat_.update(state().wind_);
        auto view = PLATFORM.screen().get_view();
        auto center = state().boat_.get_position();
        center.x -= 120.0_fixed;
        center.y -= 80.0_fixed;
        view.set_center(fvec(center));
        PLATFORM.screen().set_view(view);

        if (state().circ_radius_ < 0) {
            show_hud(state().wind_);
        }

        if (button_down<Button::action_2>()) {
            PLATFORM.screen().schedule_fade(1);
            PLATFORM.fill_overlay(0);
            state().exit_ = true;
            APP.effects().clear();
            return make_scene<DataCartModule>();
        }

        return null_scene();
    }


    void show_hud(const Fixnum& wind)
    {
        for (int x = 0; x < 30; ++x) {
            PLATFORM.set_tile(Layer::overlay, x, 19, 0);
        }
        if (auto str = state().boat_.fmt_point_of_sail(wind)) {
            Text::print(str, {0, 19});
        }
    }


    void display() override
    {
        if (state().exit_) {
            return;
        }

        if (state().circ_radius_ >= 0) {
            int circ_center_x = PLATFORM.screen().size().x / 2;
            int circ_center_y = PLATFORM.screen().size().y / 2;

            PLATFORM_EXTENSION(iris_wipe_effect,
                               state().circ_radius_,
                               circ_center_x,
                               circ_center_y);
        } else {
            PLATFORM_EXTENSION(iris_wipe_effect, 0, 0, 0);
            PLATFORM_EXTENSION(enable_parallax_clouds, false);
        }

        if (not state().exit_) {
            state().boat_.display(state().wind_);
        }

        for (auto& effect : APP.effects()) {
            PLATFORM.screen().draw(effect->sprite());
        }

    }


private:
    Optional<DynamicMemory<State>> state_;
};



}
