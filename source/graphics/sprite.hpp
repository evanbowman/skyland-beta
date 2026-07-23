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

#include "bitvector.hpp"
#include "color.hpp"
#include "number/fixnum.hpp"
#include "number/numeric.hpp"



using TextureIndex = u16;


class Sprite
{
public:
    enum Alpha : u8 {
        opaque,
        translucent,
        transparent, // invisible
        count
    };

    enum Size : u8 { w32_h32, w16_h32, w16_h16, w8_h8 };

    enum Flags1 : u8 {};
    enum Flags2 : u8 {};


    Sprite();


    using Rotation = s16;
    using Scale = Vec2<s16>;


    void set_rotation(Rotation rot);


    void set_scale(const Scale& scale);


    void set_position(const Vec2<Fixnum>& position);


    void set_origin(const Vec2<s16>& origin);


    void set_texture_index(TextureIndex texture_index);


    Vec2<bool> get_flip() const
    {
        return {flip_x_, flip_y_};
    }


    Alpha get_alpha() const
    {
        return static_cast<Sprite::Alpha>(alpha_);
    }


    void set_mix(const ColorMix& mix)
    {
        // NOTE: struct copy assignment was generating a memcpy call
        mix_.color_ = mix.color_;
        mix_.amount_ = mix.amount_;
    }


    void set_size(Size size);


    const Vec2<Fixnum>& get_position() const
    {
        return position_;
    }


    const Vec2<s16>& get_origin() const
    {
        return origin_;
    }


    TextureIndex get_texture_index() const
    {
        return texture_index_;
    }


    void move_y(const Fixnum& dy)
    {
        position_.y += dy;
    }


    void move_x(const Fixnum& dx)
    {
        position_.x += dx;
    }


    void set_alpha(Alpha alpha);


    void set_flip(const Vec2<bool>& flip);


    void set_tidx_8x8(u16 begin_index_16x32, u16 offset)
    {
        set_texture_index(begin_index_16x32 * 8 + offset);
    }


    void set_tidx_16x16(u16 begin_index_16x32, u16 offset)
    {
        set_texture_index(begin_index_16x32 * 2 + offset);
    }


    const ColorMix& get_mix() const
    {
        return mix_;
    }


    Size get_size() const
    {
        return static_cast<Sprite::Size>(size_);
    }


    Rotation get_rotation() const
    {
        return rot_;
    }


    const Scale& get_scale() const
    {
        return scale_;
    }


    void set_priority(u8 priority)
    {
        priority_ = priority;
    }


    u8 get_priority() const
    {
        return priority_;
    }


    void set_palette(u8 palette)
    {
        palette_ = palette;
    }


    u8 palette() const
    {
        return palette_;
    }


private:
    // For the gameboy advance edition of the game, all the data for the engine
    // is designed to fit within IWRAM, so we need to be careful about
    // memory. Packing the engine into 32kB has benefits for other platforms
    // too--this game is very cache-friendly.
    u8 alpha_ : 2;
    u8 size_ : 2;
    bool flip_x_ : 1;
    bool flip_y_ : 1;

    u8 priority_ : 2;
    u8 palette_ : 4;
    u8 reserved_ : 4;

    // Because sprites are only 16x32 or 32x32, 16bits for the origin field is
    // quite generous...
    Vec2<s16> origin_;

    Vec2<s16> scale_;

public:
    Vec2<Fixnum> position_;

private:
    Rotation rot_ = 0;
    TextureIndex texture_index_ = 0;
    ColorMix mix_;
};



// Warning: The class requires the initial mix amount passed in to be a multiple
// of five. There is a reason for this, and it has to do with smooth animations
// when the game logic is run synchronously (dt is > 1000 microseconds per
// step).
template <Microseconds Interval> class FadeColorAnimation
{
public:
    inline void advance(Sprite& sprite, Microseconds dt)
    {
        const auto& cmix = sprite.get_mix();
        if (cmix.amount_ > 0) {
            timer_ += dt;
            if (timer_ > Interval) {
                timer_ -= Interval;
                sprite.set_mix({cmix.color_, u8(cmix.amount_ - 5)});
            }
        } else {
            timer_ = 0;
            sprite.set_mix({});
        }
    }

    inline void reverse(Sprite& sprite, Microseconds dt)
    {
        const auto& cmix = sprite.get_mix();
        if (cmix.color_ not_eq ColorConstant::null and cmix.amount_ < 255) {
            timer_ += dt;
            if (timer_ > Interval) {
                timer_ -= Interval;
                sprite.set_mix({cmix.color_, u8(cmix.amount_ + 5)});
            }
        } else {
            timer_ = 0;
            sprite.set_mix({});
        }
    }

private:
    Microseconds timer_ = 0;
};
