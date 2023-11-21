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
#include "skyland/script_defs.hpp"
#include "skyland/skyland.hpp"



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
        model_.parse(file);

        if (auto r = model_.root()) {
            r->foreach_child([&](xml::Node* n) {
                if (str_eq(n->tag_, "script")) {
                    if (auto attr = n->lookup_attr("src")) {
                        app.invoke_script(attr->value_);
                    }
                }
            });
        }
    }

    repaint_model();
}



void ScriptedMenuScene::repaint_model()
{
    for (int x = 0; x < 30; ++x) {
        for (int y = 0; y < 20; ++y) {
            PLATFORM.set_tile(Layer::overlay, x, y, 0);
        }
    }

    if (auto r = model_.root()) {
        r->foreach_child([&](xml::Node* n) {
            if (n->dead_) {
                return;
            }
            if (str_eq(n->tag_, "text")) {
                u8 x = n->attr_intvalue("x");
                u8 y = n->attr_intvalue("y");
                if (n->contents_) {
                    Text::print(n->contents_,
                                OverlayCoord{x, y});
                }
            } else if (str_eq(n->tag_, "rect")) {
                u8 x = n->attr_intvalue("x");
                u8 y = n->attr_intvalue("y");
                u8 w = n->attr_intvalue("w");
                u8 h = n->attr_intvalue("h");
                u16 t = n->attr_intvalue("t");

                for (int i = x; i < x + w; ++i) {
                    for (int j = y; j < y + h; ++j) {
                        PLATFORM.set_tile(Layer::overlay, i, j, t);
                    }
                }
            } else if (str_eq(n->tag_, "md-icon")) {
                u8 x = n->attr_intvalue("x");
                u8 y = n->attr_intvalue("y");
                int icon = n->attr_intvalue("icon");
                int mem = n->attr_intvalue("mem");
                draw_image(mem, x, y, 4, 4, Layer::overlay);
                PLATFORM.load_overlay_chunk(mem, icon, 16);
            } else if (str_eq(n->tag_, "row")) {
                u8 x = n->attr_intvalue("x");
                u8 y = n->attr_intvalue("y");
                int t = n->attr_intvalue("t");
                u8 w = n->attr_intvalue("w");
                u8 p = n->attr_intvalue("p");

                for (int i = x; i < x + w; i += p) {
                    PLATFORM.set_tile(Layer::overlay, i, y, t);
                }
            } else if (str_eq(n->tag_, "col")) {
                u8 x = n->attr_intvalue("x");
                u8 y = n->attr_intvalue("y");
                int t = n->attr_intvalue("t");
                u8 h = n->attr_intvalue("w");
                u8 p = n->attr_intvalue("p");

                for (int i = y; i < y + h; i += p) {
                    PLATFORM.set_tile(Layer::overlay, x, i, t);
                }
            }
        });
    }
}



void ScriptedMenuScene::exit(App& app, Scene& next)
{
    ActiveWorldScene::exit(app, next);

    invoke_hook(app, "on-menu-exit");
    PLATFORM.fill_overlay(0);
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

    if (needs_repaint_) {
        repaint_model();
    }

    return null_scene();
}



void ScriptedMenuScene::display(App& app)
{
    ActiveWorldScene::display(app);
}



void ScriptedMenuScene::gui_add_node(const char* parent_id,
                                     const char* id,
                                     const char* type)
{
    xml::Node* n = model_.root();
    if (parent_id) {
        n = xml::find_by_attr(model_.root(), "id", parent_id);
    }

    if (not n) {
        return;
    }

    if (auto c = model_.add_child(n, type)) {
        model_.set_attribute(c, "id", id);
    }

    needs_repaint_ = true;
}



void ScriptedMenuScene::gui_delete_node(const char* id)
{
    if (auto n = xml::find_by_attr(model_.root(), "id", id)) {
        n->dead_ = true;
        needs_repaint_ = true;
    }
}



void ScriptedMenuScene::gui_set_attr(const char* id,
                                     const char* attr,
                                     const char* value)
{
    if (auto n = xml::find_by_attr(model_.root(), "id", id)) {
        model_.set_attribute(n, attr, value);
        needs_repaint_ = true;
    } else {
        Platform::fatal(format("missing id %", id));
    }
}



void ScriptedMenuScene::gui_set_content(const char* id, const char* content)
{
    if (auto n = xml::find_by_attr(model_.root(), "id", id)) {
        model_.set_contents(n, content);
        needs_repaint_ = true;
    }
}



} // namespace skyland
