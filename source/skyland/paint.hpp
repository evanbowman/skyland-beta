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

#include "skyland/scene/module.hpp"



// Implements a simplistic paint program.



namespace skyland
{



class Paint
{
public:
    Paint(u8 canvas_width = 13,
          u8 canvas_height = 11,
          u8 canvas_origin_x = 3,
          u8 canvas_origin_y = 3,
          int view_shift = -18)
        : view_shift_(view_shift), width_(canvas_width), height_(canvas_height),
          origin_x_(canvas_origin_x), origin_y_(canvas_origin_y)
    {
    }


    virtual ~Paint()
    {
    }


    ScenePtr update(Time delta);


    void display();


    void init();


    virtual void show();
    void draw_rulers();
    void show_color_name();
    void show_toolbar();
    void show_preview();


    void apply_drag(int xo, int yo, bool record_history = true);


    virtual u8 get_pixel(u8 x, u8 y) = 0;
    virtual void set_pixel(u8 x, u8 y, u8 value) = 0;


    u8 width() const
    {
        return width_;
    }


    u8 height() const
    {
        return height_;
    }


protected:
    Time cursor_move_tic_ = 0;
    s16 view_shift_;

    u16 palette_[16];

    bool ready_ : 1 = false;
    bool flicker_on_ : 1 = false;
    bool draw_world_ : 1 = true;
    bool preview_ : 1 = false;
    bool copy_tool_txtr_ : 1 = true;


    static Vec2<u8> cursor_;
    static u32 color_;


    u8 width_;
    u8 height_;
    u8 origin_x_;
    u8 origin_y_;
    u8 tool_selector_anim_ = 0;

    enum class Tool : u8 {
        pen,
        bucket,
        drag,
        undo,
        preset,
        exit,
        count,
    };

    static Tool tool_;

    enum class Mode : u8 {
        draw,
        tool_select,
    } mode_ = Mode::draw;

    u8 cursor_flicker_ = 0;

    Tool last_tool_ = Tool::pen;

    struct HistoryEntry
    {
        u8 type_;

        struct PixelChanged
        {
            u8 x_ : 4;
            u8 y_ : 4;
            u8 prev_color_ : 4;
        };

        struct CanvasDragged
        {
            u8 dir_;
        };

        struct BucketFill
        {
            u8 pixels_filled_;
        };

        union
        {
            PixelChanged pen_;
            CanvasDragged drag_;
            BucketFill bucket_;
        };
    };

    void push_history(HistoryEntry::PixelChanged p);
    void push_history(HistoryEntry::CanvasDragged d);
    void push_history(HistoryEntry::BucketFill b);
    void push_history(HistoryEntry h);
    bool undo(bool repaint = true);

    void show_tool_name();

    using HistoryBuffer = Buffer<HistoryEntry, 678>;
    Optional<DynamicMemory<HistoryBuffer>> history_;
};



} // namespace skyland
