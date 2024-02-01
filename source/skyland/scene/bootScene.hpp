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

#include "fadeInScene.hpp"
#include "modules/datetimeModule.hpp"
#include "modules/macrocosmFreebuildModule.hpp"
#include "platform/conf.hpp"
#include "platform/flash_filesystem.hpp"
#include "script/lisp.hpp"
#include "skyland/latency.hpp"
#include "skyland/player/coOpTeam.hpp"
#include "skyland/scene/introCreditsScene.hpp"
#include "skyland/scene/modules/skylandForever.hpp"
#include "skyland/scene_pool.hpp"
#include "skyland/skyland.hpp"
#include "version.hpp"



namespace skyland
{



static constexpr const char* lang_file = "/lang.txt";



void init_clouds();



void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



class LanguageSelectScene : public Scene
{
private:
    using LanguageOptions =
        Buffer<std::pair<StringBuffer<48>, StringBuffer<48>>, 16>;
    DynamicMemory<LanguageOptions> opts_;

    int sel_ = 0;

    Buffer<Text, 8> text_opts_;

    bool clean_boot_;

public:
    LanguageSelectScene(bool clean_boot)
        : opts_(load_language_options()), clean_boot_(clean_boot)
    {
    }


    void enter(Scene& prev) override
    {
        if (opts_->size() > 1) {
            u8 row = 3;
            for (auto& opt : *opts_) {
                text_opts_.emplace_back(opt.first.c_str(),
                                        OverlayCoord{3, row});
                row += 2;
            }
        }
    }


    void exit(Scene& prev) override
    {
        text_opts_.clear();
        for (int y = 0; y < 20; ++y) {
            PLATFORM.set_tile(Layer::overlay, 1, y, 0);
        }
    }


    ScenePtr<Scene> update(Time delta) override
    {
        auto show_cursor = [&] {
            for (int y = 0; y < 20; ++y) {
                PLATFORM.set_tile(Layer::overlay, 1, y, 0);
            }
            PLATFORM.set_tile(Layer::overlay, 1, 3 + sel_ * 2, 396);
        };

        show_cursor();

        if (key_down<Key::up>()) {
            if (sel_ > 0) {
                --sel_;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        } else if (key_down<Key::down>()) {
            if (sel_ < (int)opts_->size() - 1) {
                ++sel_;
                PLATFORM.speaker().play_sound("click_wooden", 2);
            }
        } else if (opts_->empty() or opts_->size() == 1 or
                   key_down<Key::action_1>()) {
            if (opts_->size() > 1) {
                auto path = (*opts_)[sel_].second.c_str();
                systemstring_bind_file(path);
                flash_filesystem::store_file_data(
                    lang_file, path, strlen(path));
            }
            auto has_clock = PLATFORM.system_clock().initial_time();
            if (clean_boot_ and has_clock) {
                auto next = scene_pool::alloc<DatetimeModule>();
                next->next_scene_ =
                    scene_pool::make_deferred_scene<IntroCreditsScene>();
                return next;
            } else {
                if (PLATFORM.device_name() == "MacroDesktopDemo") {
                    APP.gp_.stateflags_.set(
                        GlobalPersistentData::freebuild_unlocked, true);
                    return scene_pool::alloc<MacrocosmFreebuildModule>();
                }

                return scene_pool::alloc<IntroCreditsScene>();
            }
        }

        return null_scene();
    }


private:
    static DynamicMemory<LanguageOptions> load_language_options()
    {
        auto result = allocate_dynamic<LanguageOptions>("lang-table");

        auto cp = PLATFORM.load_file_contents("strings", "lang.txt");

        std::pair<StringBuffer<48>, StringBuffer<48>> current;
        int parse_state = 0;
        utf8::scan(
            [&](utf8::Codepoint cp, const char* raw, int) {
                if (cp == '=') {
                    parse_state = 1;
                } else if (cp == '\n') {
                    parse_state = 0;
                    result->emplace_back(current);
                    current.first.clear();
                    current.second.clear();
                } else {
                    if (parse_state == 0) {
                        current.first += raw;
                    } else {
                        current.second += raw;
                    }
                }
            },
            cp,
            strlen(cp));

        return result;
    }
};



class BootScene : public Scene
{
public:
    bool clean_boot_;

    static constexpr const auto amber_color = custom_color(0xfce165);
    static constexpr const auto back_color = custom_color(0x00210f);


    bool diagnostic_ = false;


    BootScene(bool clean_boot) : clean_boot_(clean_boot)
    {
    }



    static void init()
    {
        PLATFORM.screen().schedule_fade(1.f, ColorConstant::silver_white);
        PLATFORM.screen().clear();
        PLATFORM.screen().display();


        PLATFORM.system_call("vsync", 0);
        PLATFORM.enable_glyph_mode(true);
        PLATFORM.load_overlay_texture("overlay");

        auto fc = FontColors{amber_color, back_color};

        for (u8 x = 0; x < 30; ++x) {
            for (u8 y = 0; y < 3; ++y) {
                Text::print(" ", {x, y}, fc);
            }
            for (u8 y = 15; y < 20; ++y) {
                Text::print(" ", {x, y}, fc);
            }
        }

        __draw_image(1, 0, 0, 30, 12, Layer::map_1);
        PLATFORM.load_tile1_texture("boot_img_flattened");



        PLATFORM.screen().schedule_fade(0.f);

        const auto st = calc_screen_tiles();

        __draw_image(1, 0, 3, 30, 12, Layer::map_1);
        PLATFORM.screen().clear();
        PLATFORM.screen().display();


        PLATFORM.screen().schedule_fade(
            1.f, back_color, true, false, true, false);

        Text::print("(R)", {19, 5}, fc);



        const char* lines[] = {
            "     ",
            " .__________________________.",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            " .__________________________.",
            "                             ",
            " Cartridge Operating System ",
        };

        int i = 0;
        for (auto& l : lines) {
            Text::print(l, {0, u8(1 + i)}, fc);
            ++i;
        }

        PLATFORM.screen().clear();
        PLATFORM.screen().display();

        PLATFORM.speaker().start();
        PLATFORM.speaker().play_sound("click_digital_1", 1);


        auto vn = format("version %.%.%.%",
                         PROGRAM_MAJOR_VERSION,
                         PROGRAM_MINOR_VERSION,
                         PROGRAM_SUBMINOR_VERSION,
                         PROGRAM_VERSION_REVISION);

        PLATFORM.sleep(2);
        Text version({1, u8(st.y - 4)});
        version.append(vn.c_str(), FontColors{amber_color, back_color});
        version.__detach();

        PLATFORM.screen().clear();
        PLATFORM.screen().display();

        PLATFORM.sleep(1);
    }


    static void message(const char* text, bool log = true)
    {
        const auto st = calc_screen_tiles();

        auto fc = FontColors{amber_color, back_color};

        if (state_bit_load(StateBit::verbose_boot)) {
            Text::print(format("Mem: [%/%]",
                               scratch_buffers_in_use() * 2,
                               scratch_buffer_count * 2)
                            .c_str(),
                        {2, 8},
                        fc);

            auto stat = flash_filesystem::statistics();

            Text::print(
                format("Disk: [%/%]",
                       stat.bytes_used_ / 1024,
                       (stat.bytes_used_ + stat.bytes_available_) / 1024)
                    .c_str(),
                {16, 8},
                fc);

            u32 mstack = 0;
            PLATFORM.system_call("stack_usage", &mstack);

            Text::print(format("Stk: [%]", mstack).c_str(), {2, 10}, fc);

            for (int i = 16; i < 30; ++i) {
                PLATFORM.set_tile(Layer::overlay, i, 10, 0);
            }
            Text::print(
                format("Lisp: [%]", lisp::value_pool_info().first).c_str(),
                {16, 10},
                fc);
        }

        PLATFORM.system_call("vsync", 0);
        Text msg({1, u8(st.y - 2)});
        msg.append(text, fc);
        auto len = msg.len();
        for (int x = 0; x < st.x - len; ++x) {
            msg.append(" ", fc);
        }
        msg.__detach();

        if (log) {
            info(text);
        }

        PLATFORM.screen().clear();
        PLATFORM.screen().display();
    }


    ScenePtr<Scene> update(Time delta)
    {
        TIMEPOINT(t1);

        PLATFORM.load_background_texture(
            APP.environment().background_texture());

        message("booting...", false);

        APP.init_scripts([&](const char* text) { message(text); });

        message("reticulating splines...", false);
        skyland::achievements::init();

        message("lisp gc sweep...");
        lisp::gc();

        message("ready!", false);

        TIMEPOINT(t2);

        info(format("boot took %", t2 - t1));

        Conf conf;
#define CONF_STR(NAME) (*conf.expect<Conf::String>("startup", #NAME))

        const bool show_button_hint = CONF_STR(show_button_hint) == "yes";

        PLATFORM.fill_overlay(0);
        if (show_button_hint) {
            PLATFORM.system_call("vsync", 0);
            for (int y = 0; y < 20; ++y) {
                for (int x = 0; x < 30; ++x) {
                    PLATFORM.set_raw_tile(Layer::map_1, x, y, 0);
                }
            }
            PLATFORM.screen().schedule_fade(1.f, back_color, true, true, true, true);
        } else {
            PLATFORM.screen().schedule_fade(1.f);
        }

        PLATFORM.screen().clear();
        PLATFORM.screen().display();
        PLATFORM.sleep(10);

        auto fc = FontColors{amber_color, back_color};

        if (show_button_hint) {
            PLATFORM.load_tile1_texture("button_hint_flattened");

            PLATFORM.screen().schedule_fade(0.f, back_color, true, false, true, false);
            PLATFORM.screen().schedule_fade(1.f, back_color, true, false, true, false);
            __draw_image(1, 0, 3, 30, 12, Layer::map_1);

            Text::print(CONF_STR(default_btns).c_str(), {6, 1}, fc);
            Text::print(CONF_STR(next).c_str(), {6, 18}, fc);
            Text::print(CONF_STR(btn_A).c_str(), {23, 8}, fc);
            Text::print(CONF_STR(btn_B).c_str(), {23, 10}, fc);
            Text::print(CONF_STR(btn_L).c_str(), {5, 4}, fc);
            Text::print(CONF_STR(btn_R).c_str(), {23, 4}, fc);
            Text::print(CONF_STR(btn_SEL).c_str(), {9, 14}, fc);
            Text::print(CONF_STR(btn_START).c_str(), {0, 11}, fc);
            Text::print(CONF_STR(btn_DPAD).c_str(), {0, 9}, fc);

            while (1) {
                PLATFORM.keyboard().poll();
                PLATFORM.system_call("feed-watchdog", nullptr);

                if (PLATFORM.keyboard().down_transition<
                    Key::action_1,
                    Key::action_2,
                    Key::up,
                    Key::down,
                    Key::left,
                    Key::right,
                    Key::start,
                    Key::select,
                    Key::alt_1,
                    Key::alt_2>()) {
                    break;
                }

                PLATFORM.screen().clear();
                PLATFORM.screen().display();
            }

            PLATFORM.fill_overlay(0);
            PLATFORM.screen().schedule_fade(1.f);
        }

        if (not flash_filesystem::file_exists(lang_file) or clean_boot_) {
            info("lang selection...");
            return scene_pool::alloc<LanguageSelectScene>(clean_boot_);
        } else {
            message("bind strings file...");
            Vector<char> data;
            if (flash_filesystem::read_file_data(lang_file, data)) {
                StringBuffer<48> path;
                for (char c : data) {
                    path.push_back(c);
                }
                systemstring_bind_file(path.c_str());
            }
            return scene_pool::alloc<IntroCreditsScene>();
        }
    }
};



} // namespace skyland
