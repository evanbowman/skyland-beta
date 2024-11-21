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

#include "graphics/overlay.hpp"
#include "script/lisp.hpp"
#include "skyland/scene.hpp"



namespace skyland
{



class AdventureLogScene : public Scene
{
public:
    void enter(Scene& prev) override;
    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;

    lisp::Value* load_logentry(int entry);
    int logentry_count();

    StringBuffer<128> format_logentry(int entry);



    void set_next_scene(DeferredScene next)
    {
        next_ = next;
    }


    void show_page(int page_num);


private:
    Optional<DeferredScene> next_;

    enum class State {
        ready,
        page_turn_right_anim,
        page_turn_left_anim,
        page_fade_in_anim,
        fade_out,
    } state_ = State::ready;

    int page_ = 0;
    Time timer_ = 0;

    bool logbook_missing_ = false;

    int max_pages_ = 0;

    Buffer<TextView, 3> entries_;
};



} // namespace skyland
