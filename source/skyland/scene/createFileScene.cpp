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


#include "createFileScene.hpp"
#include "modules/textEditorModule.hpp"
#include "paintScene.hpp"
#include "platform/platform.hpp"
#include "skyland/skyland.hpp"



Platform::TextureCpMapper locale_texture_map();



namespace skyland
{



CreateFileScene::CreateFileScene(const char* ram_file_path)
    : file_path_(ram_file_path)
{
}


static const char* keyboard[7][7] = {{"z", "y", "g", "f", "v", "q", ";"},
                                     {"m", "b", "i", "d", "l", "j", "\""},
                                     {"w", "a", "o", "e", "u", "k", "/"},
                                     {"p", "h", "t", "n", "s", "r", "_"},
                                     {"x", "c", "(", ")", "-", " ", "."},
                                     {"$", "'", "0", "1", "2", "3", "X"},
                                     {"4", "5", "6", "7", "8", "9", "\n"}};



static const FontColors text_entry_colors{custom_color(0xffffff),
                                          custom_color(0x181835)};



TextEditorModule::SyntaxMode file_edit_mode(const StringBuffer<200>& path);



StringBuffer<16> get_extension(const StringBuffer<200>& cwd);



ScenePtr CreateFileScene::update(Time delta)
{
    if (APP.player().key_down(Key::left)) {
        if (keyboard_cursor_.x > 0) {
            --keyboard_cursor_.x;
        } else {
            keyboard_cursor_.x = 6;
        }
        render_keyboard();
    } else if (APP.player().key_down(Key::right)) {
        if (keyboard_cursor_.x < 6) {
            ++keyboard_cursor_.x;
        } else {
            keyboard_cursor_.x = 0;
        }
        render_keyboard();
    } else if (APP.player().key_down(Key::up)) {
        if (keyboard_cursor_.y > 0) {
            --keyboard_cursor_.y;
        } else {
            keyboard_cursor_.y = 6;
        }
        render_keyboard();
    } else if (APP.player().key_down(Key::down)) {
        if (keyboard_cursor_.y < 6) {
            ++keyboard_cursor_.y;
        } else {
            keyboard_cursor_.y = 0;
        }
        render_keyboard();
    } else if (APP.player().key_down(Key::action_1)) {
        const char c = keyboard[keyboard_cursor_.y][keyboard_cursor_.x][0];
        path_.push_back(c);
        auto temp = path_;
        while (not temp.full()) {
            temp.push_back(' ');
        }
        entry_->assign(temp.c_str(), text_entry_colors);

    } else if (APP.player().key_down(Key::action_2)) {
        if (not path_.empty()) {
            path_.pop_back();
            auto temp = path_;
            while (not temp.full()) {
                temp.push_back(' ');
            }
            entry_->assign(temp.c_str(), text_entry_colors);
        } else {
            // TODO: exit
        }
    } else if (APP.player().key_down(Key::start)) {
        if (not path_.empty()) {
            StringBuffer<100> full_path_(file_path_.c_str());
            full_path_ += path_;

            if (get_extension(full_path_) == ".img") {
                return make_scene<PaintScene>(full_path_.c_str(), true);
            } else {
                UserContext ctx;

                return make_scene<TextEditorModule>(

                    std::move(ctx),
                    full_path_.c_str(),
                    file_edit_mode(full_path_),
                    TextEditorModule::FileMode::create);
            }
        }
    }

    return null_scene();
}


static const auto status_colors =
    FontColors{custom_color(0x000010), custom_color(0xffffff)};


void CreateFileScene::render_keyboard()
{
    for (int x = 0; x < 7; ++x) {
        for (int y = 0; y < 7; ++y) {
            const char c = keyboard[y][x][0];
            auto mapping_info = locale_texture_map()(c);
            const u16 t = PLATFORM.map_glyph(c, *mapping_info);

            auto colors = status_colors;
            if (x == keyboard_cursor_.x and y == keyboard_cursor_.y) {
                colors = FontColors{custom_color(0xffffff),
                                    ColorConstant::aerospace_orange};
            }

            PLATFORM.set_tile((30 - 8) + x, (19 - 6) + y, t, colors);
        }
    }
}


void CreateFileScene::enter(Scene& prev)
{
    render_keyboard();

    title_text_.emplace("create file:", OverlayCoord{1, 1});
    entry_.emplace(OverlayCoord{1, 4});
    entry_->assign(StringBuffer<28>(' ', 28).c_str(), text_entry_colors);
}



void CreateFileScene::exit(Scene& next)
{
    title_text_.reset();
    entry_.reset();

    PLATFORM.fill_overlay(0);
}



} // namespace skyland
