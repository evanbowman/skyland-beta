////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////

#pragma once

#include "graphics/overlay.hpp"
#include "skyland/scene.hpp"
#include "skyland/systemString.hpp"
#include "skyland/stateBit.hpp"



namespace skyland
{



ScenePtr simple_prompt_once(GlobalPersistentData::Flags check_flag,
                            StateBit runtime_flag,
                            const char* msg,
                            DeferredScene next);



class MenuPromptScene : public Scene
{
public:
    using OptCallback = Function<4, void()>;

    using Message = SystemStringMem;


    MenuPromptScene(const char* msg,
                    SystemString opt_1,
                    SystemString opt_2,
                    DeferredScene next,
                    OptCallback opt_1_callback,
                    OptCallback opt_2_callback)
        : msg_(allocate<Message>({"prompt-mem"}, msg)),
          next_(next),
          opt_1_(opt_1),
          opt_2_(opt_2),
          opt_1_callback_(opt_1_callback),
          opt_2_callback_(opt_2_callback)
    {
    }


    static constexpr const auto sel_colors =
        FontColors{custom_color(0x000010), custom_color(0xffffff)};


    void enter(Scene& prev) override;


    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;

private:
    DynamicMemory<Message> msg_;
    DeferredScene next_;
    SystemString opt_1_;
    SystemString opt_2_;

    Optional<TextView> text_;
    Optional<Text> t1_;
    Optional<Text> t2_;

    int cursor_ = 0;

    OptCallback opt_1_callback_;
    OptCallback opt_2_callback_;

public:
    bool play_alert_sfx_ = true;
    bool skip_unfade_ = false;
};



} // namespace skyland
