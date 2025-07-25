////////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2025 Evan Bowman
//
// This Source Code Form is subject to the terms of the Mozilla Public License,
// v. 2.0. If a copy of the MPL was not distributed with this file, You can
// obtain one at http://mozilla.org/MPL/2.0/. */
//
////////////////////////////////////////////////////////////////////////////////


#pragma once

#include "skyland/scene.hpp"



namespace skyland
{



class DebriefScene : public Scene
{
public:
    DebriefScene()
    {
    }


    ScenePtr update(Time delta);
};



} // namespace skyland
