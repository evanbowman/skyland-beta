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

#include "function.hpp"
#include "memory/uniquePtr.hpp"
#include "number/numeric.hpp"
#include "script/value.hpp"



class Platform;


namespace skyland
{


class App;
class Scene;


template <typename T> using UniqueScenePtr = UniquePtr<T, void (*)(Scene*)>;
using ScenePtr = UniqueScenePtr<Scene>;


ScenePtr null_scene();



class WorldScene;
class ConstructionScene;
class BoxedDialogSceneWS;
class MultiplayerCoOpAwaitLockScene;
class MultiplayerCoOpAwaitChrLockScene;
namespace macro
{
class MacrocosmScene;
}



class Scene
{
public:
    virtual ~Scene(){};


    virtual ScenePtr update(Time delta);


    virtual void display();


    virtual bool displays_minimap();


    virtual void enter(Scene& prev_scene);


    virtual void exit(Scene& next_scene);


    // Yeah, I should be using a visitor.
    virtual WorldScene* cast_world_scene();


    virtual macro::MacrocosmScene* cast_macrocosm_scene();


    virtual ConstructionScene* cast_construction_scene();


    virtual BoxedDialogSceneWS* cast_boxed_dialog_scene_ws();


    virtual MultiplayerCoOpAwaitLockScene* cast_co_op_await_lock_scene();


    virtual MultiplayerCoOpAwaitChrLockScene* cast_co_op_await_chr_lock_scene();


    // NOTE: gui nodes: one scene subclass in particular supports scripting menu
    // logic with an xml-styled DOM.
    virtual void
    gui_add_node(const char* parent_id, const char* id, const char* type);


    virtual void gui_delete_node(const char* id);


    virtual void
    gui_set_attr(const char* id, const char* attr, lisp::Value* val);
};


ScenePtr initial_scene(bool clean_boot);


using DeferredScene = Function<sizeof(void*) * 4, ScenePtr()>;



} // namespace skyland
