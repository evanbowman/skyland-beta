////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2022  Evan Bowman
//
// This program is free software; you can redistribute it and/or modify it under
// the terms of version 2 of the GNU General Public License as published by the
// Free Software Foundation.
//
// This program is distributed in the hope that it will be useful, but WITHOUT
// ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
// FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
// details.
//
// You should have received a copy of the GNU General Public License along with
// this program; if not, write to the Free Software Foundation, Inc., 51
// Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
//
// GPL2 ONLY. No later versions permitted.
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "environment.hpp"
#include "skyland/island.hpp"
#include "typhoon.hpp"



namespace skyland::weather
{



class Dynamic : public Typhoon
{
public:
    Dynamic(App& app);


    void update(Platform& pfrm, App& app, Microseconds delta) override;


    void rewind(Platform& pfrm, App& app, Microseconds delta) override;


    Platform::Screen::Shader shader(App& app) const override;


    void compute_palettes(App& app, u8 scale);
};



} // namespace skyland::weather