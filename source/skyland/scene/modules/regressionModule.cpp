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

#include "regressionModule.hpp"
#include "ext_workram_data.hpp"
#include "script/lisp.hpp"
#include "skyland/scene/selectTutorialScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



EXT_WORKRAM_DATA s8 test_index = -1;



static const auto bkg_color = custom_color(0x007cbf);
static const Text::OptColors text_colors{{custom_color(0xffffff), bkg_color}};



ScenePtr RegressionModule::update(Time delta)
{
    state_bit_store(StateBit::regression, true);

    BasicCharacter::__reset_ids();
    rng::critical_state = 5;

    if (test_index == -1) {
        PLATFORM.screen().schedule_fade(0);
        PLATFORM.screen().schedule_fade(1, bkg_color);
        PLATFORM.screen().clear();
        Text::print("please wait...", {1, 1}, text_colors);
        Text::print("running tests...", {1, 3}, text_colors);
        PLATFORM.screen().display();

        PLATFORM.walk_filesystem([](const char* path) {
            if (starts_with("/scripts/data/sounds/", StringBuffer<128>(path))) {
                while (*path not_eq '\0') {
                    ++path;
                }
                while (*path not_eq '/') {
                    --path;
                }
                ++path;
                StringBuffer<80> filename;
                while (*path not_eq '.' and *path not_eq '\0') {
                    filename.push_back(*path);
                    ++path;
                }

                auto comp = PLATFORM.get_extensions().__test_compare_sound;
                if (comp and not comp(filename.c_str())) {
                    PLATFORM.fatal(path);
                }
            }
        });

        lisp::set_var("regr-print", lisp::make_function([](int argc) {
                          L_EXPECT_ARGC(argc, 3);
                          L_EXPECT_OP(2, string);
                          L_EXPECT_OP(1, integer);
                          L_EXPECT_OP(0, integer);
                          PLATFORM.screen().clear();
                          for (int x = 0; x < 30; ++x) {
                              PLATFORM.set_tile(
                                  Layer::overlay, x, L_LOAD_INT(0), 0);
                          }
                          Text::print(lisp::get_op(2)->string().value(),
                                      {(u8)L_LOAD_INT(1), (u8)L_LOAD_INT(0)},
                                      text_colors);
                          PLATFORM.screen().display();
                          return L_NIL;
                      }));

        PLATFORM_EXTENSION(watchdog_off);
        APP.invoke_script("/scripts/data/unittest.lisp");
        APP.invoke_script("/scripts/data/apitest.lisp");
        PLATFORM_EXTENSION(watchdog_on);

        PLATFORM.screen().clear();
        Text::print("core regression passed!", {1, 1}, text_colors);
        Text::print("validating tutorials...", {1, 3}, text_colors);
        PLATFORM.screen().display();

        PLATFORM.sleep(120);

        test_index++;

        BasicCharacter::__reset_ids();

    } else {

        if (test_index > 0) {
            APP.invoke_script("/scripts/tutorials/test/common.lisp");

            auto tutorial_list =
                APP.invoke_script("/scripts/tutorials/tutorials.lisp");

            auto test_num = lisp::get_list(
                lisp::get_list(tutorial_list, test_index - 1), 2);

            APP.invoke_script(format("/scripts/tutorials/test/%.lisp",
                                     test_num->integer().value_)
                                  .c_str());
        }

        if (test_index == SelectTutorialScene::tutorial_count()) {
            PLATFORM.fill_overlay(0);
            PLATFORM.screen().schedule_fade(0);
            PLATFORM.screen().schedule_fade(1, bkg_color);
            PLATFORM.screen().clear();
            Text::print("all regression passed!", {1, 1}, text_colors);
            u32 mstack = 0;
            if (auto s = PLATFORM.get_extensions().get_stack_usage) {
                mstack = s();
            }
            Text::print(format("max stack used %", mstack).c_str(),
                        {1, 3},
                        text_colors);
            u32 ssize = 0;
            if (auto s = PLATFORM.get_extensions().get_stack_size) {
                ssize = s();
            }
            Text::print(format("(approx. stack size %)", ssize).c_str(),
                        {1, 5},
                        text_colors);
            Text::print("press any key to reset...", {1, 7}, text_colors);

            while (1) {
                PLATFORM.keyboard().poll();
                PLATFORM_EXTENSION(feed_watchdog);

                if (PLATFORM.keyboard()
                        .down_transition<Key::action_1,
                                         Key::action_2,
                                         Key::down,
                                         Key::up,
                                         Key::left,
                                         Key::right>()) {
                    PLATFORM_EXTENSION(restart);
                }

                PLATFORM.screen().clear();
                PLATFORM.screen().display();
            }
        }

        auto ret = make_scene<SelectTutorialScene>();
        ret->quick_select(test_index++);
        return ret;
    }

    return null_scene();
}



RegressionModule::Factory RegressionModule::factory_(true);



} // namespace skyland
