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


#include "sandboxResetScene.hpp"
#include "modules/sandboxLoaderModule.hpp"
#include "skyland/skyland.hpp"
#include "titleScreenScene.hpp"



namespace skyland
{



static const auto sel_colors =
    FontColors{custom_color(0x000010), custom_color(0xffffff)};



ScenePtr SandboxResetScene::update(Time delta)
{
    if (APP.player().key_down(Key::up)) {
        selection_ = true;
        yes_text_->assign(SYSTR(yes)->c_str(), sel_colors);
        no_text_->assign(SYSTR(exit)->c_str());
    }

    if (APP.player().key_down(Key::down)) {
        selection_ = false;
        yes_text_->assign(SYSTR(yes)->c_str());
        no_text_->assign(SYSTR(exit)->c_str(), sel_colors);
    }

    if (APP.player().key_down(Key::action_1)) {
        if (selection_) {
            return make_scene<SandboxLoaderModule>();
        } else {
            return make_scene<TitleScreenScene>(3);
        }
    }

    return null_scene();
}


void SandboxResetScene::enter(Scene& prev)
{
    msg_.emplace(SYSTR(reset_sandbox_query)->c_str(), OverlayCoord{1, 1});
    yes_text_.emplace(OverlayCoord{2, 3});
    no_text_.emplace(SYSTR(exit)->c_str(), OverlayCoord{2, 5});

    yes_text_->assign(SYSTR(yes)->c_str(), sel_colors);
}



void SandboxResetScene::exit(Scene& next)
{
    msg_.reset();
    yes_text_.reset();
    no_text_.reset();
}



} // namespace skyland
