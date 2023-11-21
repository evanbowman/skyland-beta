////////////////////////////////////////////////////////////////////////////////
//
// Copyright (C) 2023  Evan Bowman
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


#include "scriptedMenuScene.hpp"
#include "scriptHookScene.hpp"
#include "skyland/skyland.hpp"
#include "skyland/script_defs.hpp"
#include "xml.hpp"



namespace skyland
{



ScriptedMenuScene::ScriptedMenuScene(const char* script_name)
    : menu_name_(script_name)
{
}



void ScriptedMenuScene::enter(App& app, Scene& prev)
{
    ActiveWorldScene::enter(app, prev);

    StringBuffer<96> path;
    path = "/scripts/misc/gui/";
    path += menu_name_;
    path += ".xml";

    Vector<char> file;
    if (app.load_file(path.c_str(), file)) {
        xml::Model model;
        model.parse(file);

        if (auto r = model.root()) {
            r->foreach_child([&](xml::Node* n) {
                info(n->tag_);
                if (str_eq(n->tag_, "script")) {
                    if (auto attr = n->lookup_attr("src")) {
                        app.invoke_script(attr->value_);
                    }
                }

                // n->foreach_attr([&](xml::Attribute* attr) {
                //     info(format("%=%", attr->name_, attr->value_));
                // });
                // if (n->contents_) {
                //     info(n->contents_);
                // }
            });
        }
    }
}



void ScriptedMenuScene::exit(App& app, Scene& next)
{
    ActiveWorldScene::exit(app, next);

    invoke_hook(app, "on-menu-exit");
}



ScenePtr<Scene> ScriptedMenuScene::update(App& app, Microseconds delta)
{
    if (auto new_scene = ActiveWorldScene::update(app, delta)) {
        return new_scene;
    }

    if (auto next = process_script_menu_request()) {
        return next;
    }

    auto test_key = [&](Key k) {
        return app.player().test_key(k, milliseconds(500), milliseconds(100));
    };

    if (test_key(Key::left)) {
        invoke_hook(app, "on-L");
    }

    if (test_key(Key::right)) {
        invoke_hook(app, "on-R");
    }

    if (test_key(Key::up)) {
        invoke_hook(app, "on-U");
    }

    if (test_key(Key::down)) {
        invoke_hook(app, "on-D");
    }

    if (test_key(Key::action_1)) {
        invoke_hook(app, "on-A");
    }

    if (test_key(Key::action_2)) {
        invoke_hook(app, "on-B");
    }

    return null_scene();
}



void ScriptedMenuScene::display(App& app)
{
    ActiveWorldScene::display(app);
}



} // namespace skyland
