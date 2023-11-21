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


#include "scriptedMenuScene.hpp"
#include "scriptHookScene.hpp"
#include "skyland/skyland.hpp"



namespace skyland
{



ScriptedMenuScene::ScriptedMenuScene(const char* script_name)
    : menu_name_(script_name)
{
}



namespace xml
{



template <u32 max_pages>
struct GenericBumpAllocator
{
    Buffer<ScratchBufferPtr, max_pages> pages_;
    ScratchBufferPtr current_page_;
    size_t current_page_remaining_ = 0;


    GenericBumpAllocator() : current_page_(make_scratch_buffer("xml"))
    {
        pages_.emplace_back(current_page_);
        current_page_remaining_ = SCRATCH_BUFFER_SIZE;
    }


    const char* strdup(const char* str)
    {
        const auto len = str_len(str);
        if (auto cpy = (char*)alloc(len + 1, 1)) {
            for (u32 i = 0; i < len; ++i) {
                cpy[i] = str[i];
            }
            cpy[len] = '\0';
            return cpy;
        }
        return nullptr;
    }


    template<typename T>
    T* alloc()
    {
        return reinterpret_cast<T*>(alloc(sizeof(T), alignof(T)));
    }


    template<typename T>
    T* alloc_array(u32 count)
    {
        return reinterpret_cast<T*>(alloc(sizeof(T) * count, alignof(T)));
    }


    void* alloc(u32 bytes, u32 alignment)
    {
        if (bytes + alignment > current_page_remaining_) { // FIXME
            current_page_ = make_scratch_buffer("xml");
            current_page_remaining_ = SCRATCH_BUFFER_SIZE;
            pages_.emplace_back(current_page_);
        }

        void* alloc_ptr = current_page_->data_ +
            (SCRATCH_BUFFER_SIZE - current_page_remaining_);

        if (align(alignment, bytes, alloc_ptr, current_page_remaining_)) {
            current_page_remaining_ -= bytes;
            return alloc_ptr;
        }

        return nullptr;
    }
};



struct Attribute
{
    const char* name_ = nullptr;
    const char* value_ = nullptr;
};



struct Node
{
    const char* tag_ = nullptr;
    Attribute** attrs_ = nullptr;
    Node* parent_ = nullptr;
    Node** children_ = nullptr;
    const char* contents_ = nullptr;
    u16 attr_count_ = 0;
    u16 child_count_ = 0;
};



struct Model
{
    GenericBumpAllocator<10> alloc_;
    Node* root_;


    bool is_whitespace(char c)
    {
        return c == ' ' or c == '\r' or c == '\n' or c == '\t';
    }


    std::pair<Node*, u32> parse_impl(Vector<char>& file,
                                     Vector<char>::Iterator pos,
                                     Node* parent)
    {
        u32 offset = 0;

        auto step = [&] {
                        char tmp[2] = {'\0', '\0'};
                        tmp[0] = *pos;
                        ++pos;
                        ++offset;
                        info(tmp);
                    };

        auto eat_whitespace =
            [&] {
                while (is_whitespace(*pos)) {
                    if (pos == file.end()) {
                        Platform::fatal("invalid xml!");
                    }
                    step();
                }
            };

        while (pos not_eq file.end() and *pos not_eq '<') {
            step();
        }

        if (*pos not_eq '<') {
            Platform::fatal(format("invalid xml, expected <, got %",
                                   *pos));
        }

        step();

        StringBuffer<32> current_tag;
        while (pos not_eq file.end()) {
            if (*pos == '>' or is_whitespace(*pos)) {
                break;
            } else {
                current_tag.push_back(*pos);
                step();
            }
        }

        auto node = alloc_.alloc<Node>();
        node->tag_ = alloc_.strdup(current_tag.c_str());
        node->parent_ = parent;

        if (is_whitespace(*pos)) {

            Buffer<Attribute*, 20> attrs;

            // Parse attributes
            while (*pos not_eq '>') {
                if (pos == file.end()) {
                    Platform::fatal("invalid xml!");
                }
                eat_whitespace();

                StringBuffer<64> attr_buffer;
                while (*pos not_eq '=' and not is_whitespace(*pos)) {
                    attr_buffer.push_back(*pos);
                    step();
                }

                eat_whitespace();

                if (*pos not_eq '=') {
                    Platform::fatal(format("invalid xml format, expected =, got %",
                                           *pos));
                }
                step(); // skip '=' character

                const char* attr_name = alloc_.strdup(attr_buffer.c_str());
                attr_buffer.clear();

                eat_whitespace();
                if (*pos not_eq '"') {
                    Platform::fatal("invalid xml format, expected \"");
                }
                step(); // skip '"' character

                while (*pos not_eq '"') {
                    attr_buffer.push_back(*pos);
                    step();
                }
                step(); // skip '"' character

                const char* attr_value = alloc_.strdup(attr_buffer.c_str());
                attr_buffer.clear();

                Attribute* attr = alloc_.alloc<Attribute>();
                attr->name_ = attr_name;
                attr->value_ = attr_value;
                attrs.push_back(attr);

                eat_whitespace();
            }

            node->attr_count_ = attrs.size();
            node->attrs_ = alloc_.alloc_array<Attribute*>(attrs.size());
            for (u32 i = 0; i < attrs.size(); ++i) {
                node->attrs_[i] = attrs[i];
            }
        }

        Vector<Node*> children;

        while (true) {

            eat_whitespace();

            auto peek_next = pos;
            ++peek_next;

            if (*pos == '<' and *peek_next == '/') {
                // Parse the closing tag...
                step(); // skip '<' character
                step(); // skip '/' character

                const char* match = node->tag_;
                while (*pos not_eq '>') {
                    if (*pos not_eq *match) {
                        Platform::fatal(format("invalid closing tag for % (%)",
                                               node->tag_, offset));
                    }
                    step();
                    ++match;
                }
                step(); // skip '>' character
                break;
            } else if (*pos == '<') {
                auto info = parse_impl(file, pos, node);
                for (u32 i = 0; i < info.second; ++i) {
                    ++pos;
                    ++offset;
                    // step();
                }
                children.push_back(info.first);
            } else {
                step();
            }
        }

        if (not children.size() == 0) {
            node->children_ = alloc_.alloc_array<Node*>(children.size());
            for (u32 i = 0; i < children.size(); ++i) {
                node->children_[i] = children[i];
            }
            node->child_count_ = children.size();
        }

        return {node, offset};
    }


    bool parse(Vector<char>& file)
    {
        root_ = parse_impl(file, file.begin(), nullptr).first;
        return root_;
    }
};



} // namespace xml



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
        auto root = model.parse(file);
        (void)root;
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

    return null_scene();
}



void ScriptedMenuScene::display(App& app)
{
    ActiveWorldScene::display(app);
}



} // namespace skyland
