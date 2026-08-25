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

#include "allocator.hpp"
#include "graphics/overlay.hpp"
#include "graphics/spriteText.hpp"
#include "skyland/island.hpp"
#include "skyland/systemString.hpp"
#include "worldScene.hpp"



namespace skyland
{



class SelectMenuScene : public ActiveWorldScene
{
public:
    SelectMenuScene() : opts_(allocate<Options>("sel-opts"))
    {
    }


    void enter(Scene& scene) override;


    void exit(Scene& next) override;


    ScenePtr update(Time delta) override;


    void display() override;


    Island* island() const;


    using SelMenuCallback = Function<4 * sizeof(void*), ScenePtr()>;
    void register_option(SystemString name, SelMenuCallback cb);

private:
    enum class LineColoring {
        none,
        specific,
        grayed_out,
    };

    struct Options
    {
        static constexpr int cap = 10;

        Buffer<Text, cap> lines_;
        Buffer<SystemString, cap> strings_;
        Buffer<StringBuffer<16>, cap> suffixes_;
        Buffer<Function<4 * sizeof(void*), ScenePtr()>, cap> callbacks_;
        u8 longest_line_;

        Bitvector<cap> specific_;
        Bitvector<cap> grayed_out_;
        Bitvector<cap> show_coins_hint_;
        Bitvector<cap> show_power_hint_;

        Optional<SpriteText> coins_hint_;
        Optional<SpriteText> power_hint_;
        Buffer<SystemString, cap> pushed_strings_;
    };

    struct Parameters
    {
        LineColoring coloring_ = LineColoring::none;
        bool show_coins_hint_ = false;
        bool show_power_hint_ = false;
    };

    void add_line(SystemString str,
                  const char* suffix,
                  Parameters params,
                  Function<4 * sizeof(void*), ScenePtr()> callback);

    void add_line(SystemString str,
                  const char* suffix,
                  Function<4 * sizeof(void*), ScenePtr()> callback);

    void redraw_line(int line, bool highlight);

    DynamicMemory<Options> opts_;
    int sel_ = 0;
    bool show_power_on_exit_ = false;
};



} // namespace skyland
