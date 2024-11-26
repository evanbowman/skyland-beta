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


#include "factoryResetModule.hpp"
#include "platform/flash_filesystem.hpp"
#include "skyland/save.hpp"
#include "skyland/scene/titleScreenScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



ScenePtr FactoryResetModule::update(Time delta)
{
    if (not text_) {
        PLATFORM.screen().schedule_fade(0.9f);
        PLATFORM.screen().schedule_fade(1.f);
        text_.emplace();
        text_->assign(SYSTR(factory_reset)->c_str(), {1, 1}, {28, 8});
    }

    if (APP.player().key_down(Key::action_2)) {
        text_.reset();
        return make_scene<TitleScreenScene>(3);
    }

    if (APP.player().key_pressed(Key::select) and
        APP.player().key_down(Key::action_1)) {
        ++key_count_;
        if (key_count_ == 5) {
            flash_filesystem::destroy();
            PLATFORM.restart();
        }
    }

    return null_scene();
}



FactoryResetModule::Factory FactoryResetModule::factory_;



} // namespace skyland
