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


#include "switch.hpp"
#include "script/listBuilder.hpp"
#include "skyland/island.hpp"
#include "skyland/scene/setupSwitchScene.hpp"
#include "skyland/tile.hpp"



namespace skyland
{



void Switch::format_description(StringBuffer<512>& buffer)
{
    buffer += SYSTR(description_switch)->c_str();
}



void Switch::display_on_hover(Platform::Screen& screen,

                              const RoomCoord& cursor)
{
    if (not setup_) {
        return;
    }

    const auto origin = parent()->visual_origin();

    Sprite icon;
    icon.set_size(Sprite::Size::w16_h32);

    auto pos1 = origin;
    pos1.x += branch_1_.x * 16;
    pos1.y += branch_1_.y * 16;
    icon.set_position(pos1);
    icon.set_texture_index(49);
    if (not on_) {
        icon.set_alpha(Sprite::Alpha::translucent);
    }
    screen.draw(icon);

    auto pos2 = origin;
    pos2.x += branch_2_.x * 16;
    pos2.y += branch_2_.y * 16;
    icon.set_position(pos2);
    icon.set_texture_index(50);
    if (on_) {
        icon.set_alpha(Sprite::Alpha::translucent);
    } else {
        icon.set_alpha(Sprite::Alpha::opaque);
    }
    screen.draw(icon);
}



ScenePtr Switch::select(const RoomCoord& cursor)
{
    if (not setup_) {
        return make_scene<SetupSwitchScene>(position());
    }

    if (RoomCoord{u8(cursor.x - 1), cursor.y} == position()) {
        on_ = not on_;
        parent()->repaint();
    } else {
        if (on_) {
            if (auto room = parent()->get_room(branch_1_)) {
                if (room not_eq this) {
                    room->select(room->position());
                }
            }
        } else {
            if (auto room = parent()->get_room(branch_2_)) {
                if (room not_eq this) {
                    room->select(room->position());
                }
            }
        }
    }

    return null_scene();
}



Switch::Switch(Island* parent, const RoomCoord& position)
    : Decoration(parent, name(), position)
{
}



lisp::Value* Switch::serialize()
{
    lisp::ListBuilder builder;

    builder.push_back(L_SYM(name()));
    builder.push_back(L_INT(position().x));
    builder.push_back(L_INT(position().y));

    builder.push_back(L_CONS(L_INT(branch_1_.x), L_INT(branch_1_.y)));

    builder.push_back(L_CONS(L_INT(branch_2_.x), L_INT(branch_2_.y)));

    if (health() not_eq max_health()) {
        builder.push_back(lisp::make_integer(health()));
    }

    return builder.result();
}



void Switch::deserialize(lisp::Value* list)
{
    if (lisp::length(list) >= 6) {
        __set_health(lisp::get_list(list, 5)->integer().value_);
    }

    if (lisp::length(list) >= 5) {
        auto p1 = lisp::get_list(list, 3);
        auto p2 = lisp::get_list(list, 4);

        if (p1->type() == lisp::Value::Type::cons and
            p2->type() == lisp::Value::Type::cons) {

            setup_ = true;
            branch_1_.x = p1->cons().car()->integer().value_;
            branch_1_.y = p1->cons().cdr()->integer().value_;

            branch_2_.x = p2->cons().car()->integer().value_;
            branch_2_.y = p2->cons().cdr()->integer().value_;
        }
    }
}



void Switch::render_interior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = InteriorTile::switch_1;

    if (on_) {
        buffer[position().x + 1][position().y] = InteriorTile::switch_on;
    } else {
        buffer[position().x + 1][position().y] = InteriorTile::switch_off;
    }
}



void Switch::render_exterior(App* app, TileId buffer[16][16])
{
    buffer[position().x][position().y] = Tile::switch_1;

    if (on_) {
        buffer[position().x + 1][position().y] = Tile::switch_on;
    } else {
        buffer[position().x + 1][position().y] = Tile::switch_off;
    }
}



} // namespace skyland
