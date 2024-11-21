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


#include "dustStorm.hpp"
#include "containers/vector.hpp"
#include "skyland/island.hpp"
#include "skyland/sharedVariable.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void init_clouds();



} // namespace skyland



namespace skyland::weather
{



EnvironmentId DustStorm::id() const
{
    return id_;
}



DustStorm::DustStorm()
{
    auto& s = *state_;
    s.particle_count_ = 12;

    const auto scale = 128;

    for (int i = 0; i < s.particle_count_; ++i) {
        auto& rd = s.raindrops_[i];
        rd.x = rng::choice(240 * scale, rng::utility_state);
        rd.y = rng::choice(160 * scale, rng::utility_state);
    }

    s.spr_ += 4;

    for (int x = 0; x < 32; ++x) {
        for (int y = 14; y < 32; ++y) {
            PLATFORM.set_tile(Layer::background, x, y, 4);
        }
    }
}



DustStorm::~DustStorm()
{
    init_clouds();
}



void DustStorm::display()
{
    if (not PLATFORM.screen().fade_active()) {
        (*state_).display();
    }
}



SHARED_VARIABLE(dust_storm_damage);



void DustStorm::update(Time delta)
{
    (*state_).update(clamp(delta * 2, Time(0), milliseconds(32)));

    damage_timer_ += delta;
    if (damage_timer_ > seconds(4)) {
        damage_timer_ -= seconds(4);

        if (PLATFORM.screen().fade_active()) {
            return;
        }
        if (APP.opponent_island() and APP.opponent().is_friendly()) {
            // Yeah, we don't want to damage islands associated with neutral
            // events...
            return;
        }

        Vector<Room*> tmp;

        collect_outer_rooms(APP.player_island(), tmp);

        for (auto& d : APP.player_island().drones()) {
            d->apply_damage(dust_storm_damage);
        }

        if (APP.opponent_island()) {
            collect_outer_rooms(*APP.opponent_island(), tmp);

            for (auto& d : APP.opponent_island()->drones()) {
                d->apply_damage(dust_storm_damage);
            }
        }

        for (auto& r : tmp) {
            r->apply_damage(dust_storm_damage);
        }
    }
}



void DustStorm::rewind(Time delta)
{
    (*state_).rewind(delta);
}



Platform::Screen::Shader DustStorm::shader() const
{
    return [](ShaderPalette palette, ColorConstant k, int arg, int index) {
        switch (palette) {
        case ShaderPalette::tile0:
            switch (index & 0x0f) {
            case 1:
                return custom_color(0x134739);
            case 2:
                return custom_color(0x69786e);
            case 3:
                return custom_color(0x9fc9b6);
            case 5:
                return custom_color(0xd46c26);
            case 8:
                return custom_color(0xcced85);
            case 9:
                return custom_color(0xeafad9);
            case 10:
                if (not player_island().interior_visible()) {
                    return custom_color(0xc1debf);
                }
                break;
            case 12:
                return custom_color(0x1567C6);
            case 13:
                return custom_color(0xDBE4A4);
            case 14:
                return custom_color(0xA9B37E);
            case 15:
                return custom_color(0x676C3A);
            }
            break;

        case ShaderPalette::tile1:
            switch (index) {
            case 1:
                return custom_color(0x134739);
            case 2:
                return custom_color(0x69786e);
            case 3:
                return custom_color(0x9fc9b6);
            case 4:
                if (opponent_island() and
                    not opponent_island()->interior_visible()) {
                    return custom_color(0x394854);
                }
                break;
            case 5:
                return custom_color(0xd46c26);
            case 8:
                return custom_color(0xcced85);
            case 9:
                return custom_color(0xeafad9);
            case 10: // FIXME
                if (opponent_island() and
                    not opponent_island()->interior_visible()) {
                    return custom_color(0x949366);
                }
                break;
            case 12:
                return custom_color(0x1567C6);
            case 13:
                return custom_color(0xDBE4A4);
            case 14:
                return custom_color(0xA9B37E);
            case 15:
                return custom_color(0x676C3A);
            }
            break;

        case ShaderPalette::background:
            switch (index) {
            case 1:
                return custom_color(0xacbd99);
            case 2:
                return custom_color(0xeafad9);
            case 3:
                return custom_color(0xc1debf);
            case 4:
                return custom_color(0x76ad74);
            case 5:
                return custom_color(0x81c7c3);
            }
            break;

        case ShaderPalette::spritesheet:
            switch (index) {

            case 8:
                return custom_color(0x5e758c);

            case 10:
                return custom_color(0x0872E9);

            case 11:
                return custom_color(0x78b8bf);

            case 12:
                return custom_color(0x134739);

            default:
                break;
            }
            break;

        default:
            return k;
        }

        return k;
    };
}



} // namespace skyland::weather
