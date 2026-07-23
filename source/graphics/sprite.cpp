////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2023 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#include "sprite.hpp"


Sprite::Sprite()
    : alpha_(Alpha::opaque), size_(Size::w32_h32), flip_x_(false),
      flip_y_(false)
{
    priority_ = 1;
    palette_ = 0;
}


void Sprite::set_scale(const Scale& scale)
{
    scale_ = scale;
}


void Sprite::set_rotation(Rotation rot)
{
    rot_ = rot;
}


void Sprite::set_position(const Vec2<Fixnum>& position)
{
    position_ = position;
}


void Sprite::set_origin(const Vec2<s16>& origin)
{
    origin_ = origin;
}


void Sprite::set_texture_index(TextureIndex texture_index)
{
    texture_index_ = texture_index;
}


void Sprite::set_flip(const Vec2<bool>& flip)
{
    flip_x_ = flip.x;
    flip_y_ = flip.y;
}


void Sprite::set_alpha(Alpha alpha)
{
    alpha_ = alpha;
}


void Sprite::set_size(Size size)
{
    size_ = size;
}
