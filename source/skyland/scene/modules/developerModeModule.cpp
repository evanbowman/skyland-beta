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


#include "developerModeModule.hpp"
#include "containers/vector.hpp"
#include "platform/flash_filesystem.hpp"
#include "skyland/dlc.hpp"
#include "skyland/scene/fullscreenDialogScene.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);


static const FontColors text_colors{custom_color(0xcec6ef),
                                    custom_color(0x163061)};


static const FontColors dev_opt_colors = {
    custom_color(0x163061),
    custom_color(0x45c496),
};



void DeveloperModeModule::set_opt(bool value)
{
    option_ = value;

    if (option_) {
        option_text_->assign("< ", text_colors);
        option_text_->append(SYSTR(yes)->c_str(), dev_opt_colors);
        option_text_->append(" >", text_colors);
    } else {
        option_text_->assign("< ", text_colors);
        option_text_->append(SYSTR(no)->c_str(), dev_opt_colors);
        option_text_->append(" >", text_colors);
    }
}



void DeveloperModeModule::enter(Scene& prev)
{
    message_.emplace();
    message_overflow_.emplace();

    was_developer_mode_ = APP.is_developer_mode();

    option_text_.emplace(OverlayCoord{3, 16});
    set_opt(was_developer_mode_);

    auto str = SYSTR(developer_mode_msg);

    PLATFORM.speaker().set_music_volume(8);


    message_->assign(str->c_str(), {1, 1}, {28, 6}, 0, text_colors);

    message_overflow_->assign(
        str->c_str() + message_->parsed(), {1, 7}, {12, 9}, 0, text_colors);


    PLATFORM.screen().set_shader(passthrough_shader);
    PLATFORM.load_tile0_texture("developer_mode_flattened");
    PLATFORM.screen().schedule_fade(0.f, custom_color(0x163061));

    for (int y = 0; y < 16; ++y) {
        for (int x = 0; x < 16; ++x) {
            PLATFORM.set_tile(Layer::map_0_ext, x, y, 1);
            PLATFORM.set_tile(Layer::map_1_ext, x, y, 0);
        }
    }
    __draw_image(31, 0, 6, 30, 13, Layer::map_0);
}



void DeveloperModeModule::exit(Scene& next)
{
    PLATFORM.fill_overlay(0);

    PLATFORM.speaker().set_music_volume(Platform::Speaker::music_volume_max);

    show_island_exterior(&player_island());
}



ScenePtr DeveloperModeModule::update(Time delta)
{
    if (exit_) {
        PLATFORM.screen().schedule_fade(1.f);
        return make_scene<TitleScreenScene>(3);
    }


    if (APP.player().key_down(Key::action_1) or
        APP.player().key_down(Key::action_2)) {

        if (was_developer_mode_ not_eq option_) {
            APP.set_developer_mode(not APP.is_developer_mode());
            save::store_global_data(APP.gp_);
        }

        message_.reset();
        message_overflow_.reset();
        option_text_.reset();
        exit_ = true;
        PLATFORM.screen().schedule_fade(1.f);
    }

    if (APP.player().key_down(Key::right) or APP.player().key_down(Key::left)) {
        set_opt(not option_);
    }

    return null_scene();
}



DeveloperModeModule::Factory DeveloperModeModule::factory_;



} // namespace skyland
