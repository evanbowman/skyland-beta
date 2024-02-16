////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2024  Evan Bowman. Some rights reserved.
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

#include "setLanguageScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



void LanguageSelectScene::redraw_title()
{
    title_.emplace(OverlayCoord{1, 1});

    auto l = get_line_from_file(
        format("/strings/%.idf", (*opts_)[sel_].second.c_str()).c_str(),
        (int)SystemString::set_language + 1);

    title_->assign(l->c_str(),
                   Text::OptColors{{ColorConstant::silver_white,
                                    ColorConstant::steel_blue}});
}



void LanguageSelectScene::enter(Scene& prev)
{
    PLATFORM.screen().schedule_fade(1.f, ColorConstant::silver_white);

    if (opts_->size() > 1) {
        u8 row = 3;
        for (auto& opt : *opts_) {

            text_opts_.emplace_back(OverlayCoord{3, row});
            text_opts_.back().assign(
                opt.first.c_str(),
                Text::OptColors{
                    {ColorConstant::steel_blue, ColorConstant::silver_white}});
            row += 2;
        }
    }

    redraw_title();
}



void LanguageSelectScene::exit(Scene& prev)
{
    text_opts_.clear();
    title_.reset();

    for (int y = 3; y < 20; ++y) {
        PLATFORM.set_tile(Layer::overlay, 1, y, 0);
    }

    PLATFORM.screen().fade(1.f, ColorConstant::rich_black, {}, true, true);
}



ScenePtr<Scene> LanguageSelectScene::update(Time delta)
{
    auto show_cursor = [&] {
        for (int y = 3; y < 20; ++y) {
            PLATFORM.set_tile(Layer::overlay, 1, y, 0);
        }
        PLATFORM.set_tile(Layer::overlay, 1, 3 + sel_ * 2, 397);
    };

    show_cursor();

    if (key_down<Key::up>()) {
        if (sel_ > 0) {
            --sel_;
            PLATFORM.speaker().play_sound("click_wooden", 2);
            redraw_title();
        }
    } else if (key_down<Key::down>()) {
        if (sel_ < (int)opts_->size() - 1) {
            ++sel_;
            PLATFORM.speaker().play_sound("click_wooden", 2);
            redraw_title();
        }
    } else if (opts_->empty() or opts_->size() == 1 or
               key_down<Key::action_1>()) {
        if (opts_->size() > 1) {
            auto path = (*opts_)[sel_].second.c_str();
            systemstring_bind_language(path);
            flash_filesystem::store_file_data(lang_file, path, strlen(path));
        }
        auto has_clock = PLATFORM.system_clock().initial_time();
        if (clean_boot_ and has_clock) {
            auto next = make_scene<DatetimeModule>();
            next->next_scene_ = make_deferred_scene<IntroCreditsScene>();
            return next;
        } else {
            if (next_) {
                return (*next_)();
            } else {
                return make_scene<IntroCreditsScene>();
            }
        }
    }

    return null_scene();
}



DynamicMemory<LanguageSelectScene::LanguageOptions>
LanguageSelectScene::load_language_options()
{
    auto result = allocate_dynamic<LanguageOptions>("lang-table");

    auto cp = PLATFORM.load_file_contents("strings", "lang.txt");

    Pair<StringBuffer<48>, StringBuffer<48>> current;
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



} // namespace skyland
