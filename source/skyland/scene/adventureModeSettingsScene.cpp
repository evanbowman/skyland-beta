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


#include "adventureModeSettingsScene.hpp"
#include "skyland/skyland.hpp"
#include "worldMapScene.hpp"



namespace skyland
{



struct DifficultyInfo
{
    SystemString text_;
    SystemString desc_;
};



static const SystemString titles[] = {
    SystemString::sf_difficulty,
    SystemString::permadeath_setting,
};



static const DifficultyInfo difficulty_text[] = {
    {SystemString::sf_casual, SystemString::difficulty_hint_easy},
    {SystemString::sf_normal, SystemString::difficulty_hint_normal},
    {SystemString::sf_hard, SystemString::difficulty_hint_hard},
};



void AdventureModeSettingsScene::repaint_difficulty(int difficulty,
                                                    bool selected)
{
    auto d = difficulty_text[difficulty];
    render_line(0, d.text_, d.desc_, selected);
}



void AdventureModeSettingsScene::repaint_permadeath(bool on, bool selected)
{
    render_line(1,
                on ? SystemString::on : SystemString::off,
                on ? SystemString::permadeath_hint_on
                   : SystemString::permadeath_hint_off,
                selected);
}



void AdventureModeSettingsScene::render_line(int linenum,
                                             SystemString text,
                                             SystemString desc,
                                             bool selected)
{
    StringBuffer<96> result;
    auto title = loadstr(titles[linenum]);
    result += title->c_str();
    result += ": ";

    size_t max_title = 0;
    for (u32 i = 0; i < sizeof(titles) / sizeof(titles[0]); ++i) {
        max_title = std::max(max_title, utf8::len(loadstr(titles[i])->c_str()));
    }

    const auto title_len = utf8::len(title->c_str());
    for (size_t i = 0; i < max_title - title_len; ++i) {
        result += " ";
    }

    result += loadstr(text)->c_str();

    auto& line = lines_[linenum];
    line.set_coord(OverlayCoord{2, (u8)(4 + linenum * 2)});

    line.assign(loadstr(titles[linenum])->c_str());
    line.append(": ");

    for (size_t i = 0; i < max_title - title_len; ++i) {
        line.append(" ");
    }

    static const auto highlight_colors =
        Text::OptColors{{custom_color(0xffffff), custom_color(0x406e98)}};

    auto clr = highlight_colors;
    if (not selected) {
        clr = nullopt();
    }

    if (selected) {
        line.append("< ", clr);
    } else {
        line.append("  ");
    }

    line.append(loadstr(text)->c_str(), clr);

    if (selected) {
        line.append(" >", clr);
    }

    auto hint_colors = Text::OptColors{
        {ColorConstant::med_blue_gray, ColorConstant::rich_black}};

    if (selected) {
        desc_->assign(loadstr(desc)->c_str(), {1, 14}, {28, 6}, 0, hint_colors);
    }
}



void AdventureModeSettingsScene::repaint()
{
    switch (sel_) {
    case 0:
        repaint_difficulty((int)APP.gp_.difficulty_, true);
        repaint_permadeath(
            APP.gp_.stateflags_.get(GlobalPersistentData::permadeath_on),
            false);
        break;

    case 1:
        repaint_difficulty((int)APP.gp_.difficulty_, false);
        repaint_permadeath(
            APP.gp_.stateflags_.get(GlobalPersistentData::permadeath_on), true);
        break;
    }
}



void AdventureModeSettingsScene::enter(Scene& prev)
{
    for (u32 i = 0; i < lines_.size() + 1; ++i) {
        lines_.emplace_back("", OverlayCoord{1, (u8)(3 + i * 2)});
    }

    auto setup_str = SYSTR(setup);
    title_.emplace(
        setup_str->c_str(),
        OverlayCoord{(u8)centered_text_margins(
                         utf8::len(setup_str->c_str()) + 1 +
                         utf8::len(SYSTR(setup_instructions)->c_str())),
                     1});

    title_->append(" ");
    title_->append(SYSTR(setup_instructions)->c_str(),
                   Text::OptColors{{ColorConstant::med_blue_gray,
                                    ColorConstant::rich_black}});

    repaint();

    for (int x = 1; x < 29; ++x) {
        PLATFORM.set_tile(Layer::overlay, x, 13, 377);
    }

    PLATFORM.screen().fade(0.96f);
    PLATFORM.screen().fade(1.f);

    original_ = (u8)APP.gp_.difficulty_;
    stateflags_cached_ = APP.gp_.stateflags_;

    desc_.emplace();
}



void AdventureModeSettingsScene::exit(Scene& prev)
{
    desc_.reset();
    title_.reset();
    lines_.clear();
    PLATFORM.fill_overlay(0);
}



void AdventureModeSettingsScene::update_field(bool inc)
{
    switch (sel_) {
    case 0: {
        auto& diff = APP.gp_.difficulty_;

        if (not inc and (int)diff == 0) {
            diff = GlobalPersistentData::Difficulty::expert;
            break;
        }
        diff = (GlobalPersistentData::Difficulty)(inc ? (int)diff + 1
                                                      : (int)diff - 1);
        if ((int)diff > (int)GlobalPersistentData::Difficulty::expert) {
            diff = GlobalPersistentData::Difficulty::beginner;
        }
        break;
    }

    case 1: {
        bool pd = APP.gp_.stateflags_.get(GlobalPersistentData::permadeath_on);
        pd = not pd;
        APP.gp_.stateflags_.set(GlobalPersistentData::permadeath_on, pd);
        break;
    }
    }
}



ScenePtr AdventureModeSettingsScene::update(Time delta)
{
    if (init_) {
        init_ = false;
    }

    APP.player().update(delta);

    auto test_key = [&](Key k) {
        return APP.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    static const int sel_max = 1;

    if (test_key(Key::up)) {
        if (sel_ > 0) {
            --sel_;
        } else {
            sel_ = sel_max;
        }
        PLATFORM.speaker().play_sound("click_wooden", 2);
        repaint();
    }

    if (test_key(Key::down)) {
        if (sel_ < sel_max) {
            ++sel_;
        } else {
            sel_ = 0;
        }
        PLATFORM.speaker().play_sound("click_wooden", 2);
        repaint();
    }

    if (test_key(Key::left)) {
        update_field(false);
        PLATFORM.speaker().play_sound("click_wooden", 2);
        repaint();
    }

    if (test_key(Key::right)) {
        update_field(true);
        PLATFORM.speaker().play_sound("click_wooden", 2);
        repaint();
    }


    if (APP.player().key_down(Key::action_1)) {
        PLATFORM.speaker().play_sound("button_wooden", 3);
        switch (APP.gp_.difficulty_) {
        case GlobalPersistentData::Difficulty::beginner:
            APP.invoke_script("/scripts/config/easy/score.lisp");
            break;

        case GlobalPersistentData::Difficulty::experienced:
            APP.invoke_script("/scripts/config/normal/score.lisp");
            break;

        case GlobalPersistentData::Difficulty::expert:
            APP.invoke_script("/scripts/config/hard/score.lisp");
            break;
        }

        if ((u8)APP.gp_.difficulty_ not_eq original_ or
            APP.gp_.stateflags_ not_eq stateflags_cached_) {
            save::store_global_data(APP.gp_);
        }

        if (newgame_) {
            APP.invoke_script("/scripts/newgame.lisp");
            if (APP.gp_.stateflags_.get(GlobalPersistentData::permadeath_on)) {
                APP.persistent_data().set_flag(PersistentData::permadeath_on);
            } else {
                APP.persistent_data().clear_flag(PersistentData::permadeath_on);
            }
        }

        return make_scene<WorldMapScene>();
    }



    return null_scene();
}



} // namespace skyland
