///////////////////////////////////////////////////////////////////////////////
//
// Copyright (c) 2024 Evan Bowman
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



void __draw_image(TileDesc start_tile,
                  u16 start_x,
                  u16 start_y,
                  u16 width,
                  u16 height,
                  Layer layer);



class DesktopOS : public Scene
{
public:


    class Clickable;


    void register_clickable(Clickable* clk)
    {
        mem_->clickables_.push_back(clk);
    }


    void unregister_clickable(Clickable* clk)
    {
        auto it = mem_->clickables_.begin();
        for (; it not_eq mem_->clickables_.end();) {
            if (*it == clk) {
                it = mem_->clickables_.erase(it);
            } else {
                ++it;
            }
        }
    }


    class Clickable
    {
    public:
        Clickable(DesktopOS* os, const HitBox::Dimension& dimension) :
            os_(os)
        {
            hitbox_.position_ = &position_;
            hitbox_.dimension_ = dimension;

            os->register_clickable(this);
        }


        Vec2<Fixnum>& pos()
        {
            return position_;
        }


        Clickable(const Clickable&) = delete;


        ~Clickable()
        {
            os_->unregister_clickable(this);
        }


        virtual void on_hover()
        {
        }


        virtual void on_click()
        {
        }


        const HitBox& hitbox() const
        {
            return hitbox_;
        }


        void set_enabled(bool enabled)
        {
            enabled_ = enabled;
        }


        bool enabled() const
        {
            return enabled_;
        }


        bool shows_pointer()
        {
            return show_pointer_;
        }


    private:
        HitBox hitbox_;
        Vec2<Fixnum> position_;

    protected:
        DesktopOS* os_;

        bool enabled_ = true;
        bool show_pointer_ = false;
    };


    class DockIcon : public Clickable
    {
    public:
        DockIcon(DesktopOS* os,
                 const char* name,
                 u8 icon_gfx,
                 Vec2<Fixnum> pos) :
            Clickable(os, {16, 16, 0, 0}),
            name_(name),
            icon_gfx_(icon_gfx)
        {
            this->pos() = pos;
            y_pos_ = this->pos().y.as_integer();
        }


        void set_minimized()
        {
            os_->minimize_window(name());
            os_->repaint_windows();
        }


        void set_open()
        {
            state_ = State::open;
            if (icon_gfx_ < 21) {
                icon_gfx_ += 8;
            }
            os_->make_window(this);
        }


        void set_closed()
        {
            state_ = State::closed;
            if (icon_gfx_ >= 21) {
                icon_gfx_ -= 8;
            }
            os_->close_window(name_);
        }


        void update()
        {
            const uint8_t dock_animation[32] = {
                0, 32, 60, 84, 104, 120, 133, 144, 153, 160, 166, 171, 175, 178, 180, 182,
                183, 182, 180, 177, 173, 168, 162, 155, 147, 138, 128, 117, 105, 92, 78, 63
            };

            switch (state_) {
            case State::closed:
                break;

            case State::opening:
                pos().y = Fixnum::from_integer(y_pos_ - dock_animation[anim_cyc_ % 32] / 24);
                anim_cyc_ += 1;
                if (anim_cyc_ == 128 - 32) {
                    anim_cyc_ = 0;
                    pos().y = y_pos_;
                    set_open();
                }
                break;

            case State::open:
                break;
            }
        }


        void on_click() override
        {
            switch (state_) {
            case State::closed:
                state_ = State::opening;
                break;

            case State::opening:
                break;

            case State::open:
                os_->focus_window(name());
                break;
            }
        }


        void on_hover() override
        {
            if (state_ == State::opening) {
                bool had_hint = (bool)os_->hint_label_;
                os_->hint_label_.reset();
                if (had_hint) {
                    os_->repaint_windows();
                }
                return;
            }
            u8 x = pos().x.as_integer() / 8;
            int offset = strlen(name_) / 2;
            x -= (offset - 2);
            OverlayCoord dest {x, 17};
            if (os_->hint_label_ and os_->hint_label_->coord() == dest) {
                return;
            }
            os_->hint_label_.emplace(name_, dest);
        }


        u8 icon_gfx() const
        {
            return icon_gfx_;
        }


        const char* name() const
        {
            return name_;
        }


    private:
        const char* name_;
        u8 icon_gfx_;
        u8 anim_cyc_ = 0;
        u8 y_pos_ = 0;

        enum class State : u8
        {
            closed,
            opening,
            open,
        } state_ = State::closed;
    };



    class Window
    {
    public:

        bool minimized_ = false;


        void minimize()
        {
            minimized_ = true;
            set_focus(false);
            pkg_->set_minimized();
        }


        class CloseButton : public Clickable
        {
        public:
            CloseButton(DesktopOS* os, Window* window) :
                Clickable(os, {7, 7, -1, -1}),
                window_(window)
            {
                this->pos().x = 3.0_fixed;
                this->pos().y = 12.0_fixed;
            }

            void on_click() override
            {
                window_->close();
            }


        private:
            Window* window_;
        };


        class MinimizeButton : public Clickable
        {
        public:
            MinimizeButton(DesktopOS* os, Window* window) :
                Clickable(os, {7, 7, -1, -1}),
                window_(window)
            {
                this->pos().x = 14.0_fixed;
                this->pos().y = 12.0_fixed;
            }

            void on_click() override
            {
                window_->minimize();
            }

        private:
            Window* window_;
        };


        Window(DesktopOS* os, DockIcon* application) : pkg_(application),
                                                       close_btn_(os, this),
                                                       minimize_btn_(os, this)
        {
        }


        virtual void destroy() {}


        void update()
        {
        }


        void close()
        {
            pkg_->set_closed();
        }


        const char* name() const
        {
            return pkg_->name();
        }


        virtual void repaint()
        {
            auto name_len = strlen(name());
            u8 mg = centered_text_margins(name_len);
            for (int x = 0; x < 30; ++x) {
                for (int y = 2; y < 17; ++y) {
                    if (y == 2) {
                        PLATFORM.set_tile(Layer::overlay, x, y, 89);
                    } else if (y == 3) {
                        PLATFORM.set_tile(Layer::overlay, x, y, 91);
                    } else {
                        PLATFORM.set_tile(Layer::overlay, x, y, 82);
                    }

                }
            }

            static constexpr const Text::OptColors heading_colors{
                {custom_color(0x717199),
                 custom_color(0xeaeef3)}};

            Text::print(name(), {mg, 3}, heading_colors);
            PLATFORM.set_tile(Layer::overlay, 0, 2, 87);
            PLATFORM.set_tile(Layer::overlay, 0, 3, 92);
            PLATFORM.set_tile(Layer::overlay, 1, 2, 94);
            PLATFORM.set_tile(Layer::overlay, 2, 2, 95);
            PLATFORM.set_tile(Layer::overlay, 2, 3, 96);
            PLATFORM.set_tile(Layer::overlay, 29, 2, 88);
        }


        virtual void set_focus(bool focused)
        {
            close_btn_.set_enabled(focused);
            minimize_btn_.set_enabled(focused);
        }


    private:
        DockIcon* pkg_;
        CloseButton close_btn_;
        MinimizeButton minimize_btn_;
    };



    class SkyTunesWindow : public Window
    {
    public:

        SkyTunesWindow(DesktopOS* os, DockIcon* application) :
            Window(os, application)
        {
            u8 i = 0;
            PLATFORM.walk_filesystem([this, os, &i](const char* path) {
                if (auto rest = starts_with("/scripts/data/music/", StringBuffer<96>(path))) {
                    StringBuffer<48> temp;
                    while (*rest not_eq '.') {
                        temp.push_back(*rest);
                        ++rest;
                    }
                    Vec2<Fixnum> pos;
                    pos.x = Fixnum::from_integer(16);
                    pos.y = Fixnum::from_integer(33 + i * 8);
                    music_opts_.emplace_back(temp.c_str(),
                                             os,
                                             HitBox::Dimension{20 * 8, 8, 0, 0},
                                             pos,
                                             this);
                    music_names_.push_back(temp.c_str());
                    ++i;
                }
            });
        }


        SkyTunesWindow(const SkyTunesWindow&) = delete;


        void destroy() override
        {
            this->~SkyTunesWindow();
        }


        ~SkyTunesWindow()
        {
            PLATFORM.speaker().stop_music();
        }


        void repaint() override
        {
            Window::repaint();

            bool alternate = false;

            static constexpr const Text::OptColors alt_colors{
                {custom_color(0x212194),
                 custom_color(0xeaeef3)}};


            for (u32 i = 0; i < music_names_.size(); ++i) {
                StringBuffer<48> nm = "  ";
                nm += music_names_[i].c_str();
                while (nm.length() < 30) {
                    nm.push_back(' ');
                }
                u8 y = u8(5 + i);
                if (alternate) {
                    Text::print(nm.c_str(), {0, y});
                    if (selected_track_ == i) {
                        PLATFORM.set_tile(Layer::overlay, 0, y, 98);
                    }
                } else {
                    Text::print(nm.c_str(), {0, y}, alt_colors);
                    if (selected_track_ == i) {
                        PLATFORM.set_tile(Layer::overlay, 0, y, 99);
                    }
                }
                alternate = not alternate;
            }
        }

        void set_focus(bool focused) override
        {
            Window::set_focus(focused);
            for (auto& opt : music_opts_) {
                opt.set_enabled(focused);
            }
        }

    private:
        class MusicTrack : public Clickable
        {
        public:
            MusicTrack(const char* name,
                       DesktopOS* os,
                       const HitBox::Dimension& dimension,
                       const Vec2<Fixnum>& pos,
                       SkyTunesWindow* window) :
                Clickable(os, dimension),
                name_(name),
                window_(window)
            {
                this->pos() = pos;
            }

            void on_click() override
            {
                auto str = name_;
                str += ".raw";
                PLATFORM.speaker().stream_music(str.c_str(), 0);
                window_->select(name_.c_str());
                os_->repaint_windows();
            }

        private:
            StringBuffer<48> name_;
            SkyTunesWindow* window_;
        };

        void select(const char* name)
        {
            for (u32 i = 0; i < music_names_.size(); ++i) {
                if (music_names_[i] == name) {
                    selected_track_ = i;
                    break;
                }
            }
        }

        Buffer<MusicTrack, 10> music_opts_;
        Buffer<StringBuffer<48>, 10> music_names_;

        u8 selected_track_ = 255;
    };


    class ExplorerWindow : public Window
    {
    public:


        using Window::Window;


        void repaint() override
        {
            Window::repaint();

            Text::print("You are not connected", {4, 8});
            Text::print("to the internet.", {4, 10});
        }
    };



    class mGBAWindow : public Window
    {
    public:

        mGBAWindow(DesktopOS* os, DockIcon* application) :
            Window(os, application),
            launch_btn_(os, this)
        {
        }


        void repaint() override
        {
            Window::repaint();

            Text::print("Recent Games:", {2, 5});

            Text::print("- Skyland.gba", {3, 7});

            static constexpr const Text::OptColors btn_colors{
                {custom_color(0xeaeef3),
                 custom_color(0x2d9773)}};

            Text::print("launch", {21, 7}, btn_colors);
        }


        class LaunchButton : public Clickable
        {
        public:
            LaunchButton(DesktopOS* os, Window* window) :
                Clickable(os, {8 * 6, 8, 0, 0}),
                window_(window)
            {
                this->pos().x = 168.0_fixed;
                this->pos().y = 49.0_fixed;
                show_pointer_ = true;
            }

            void on_click() override
            {
                os_->boot_rom();
            }


        private:
            Window* window_;
        };

    private:
        LaunchButton launch_btn_;
    };



    void boot_rom()
    {

        PLATFORM_EXTENSION(restart);
    }



    void draw_menu_bar()
    {
        for (int x = 0; x < 30; ++x) {
            PLATFORM.set_tile(Layer::overlay, x, 0, 82);
            PLATFORM.set_tile(Layer::overlay, x, 1, 82);
        }
        PLATFORM.set_overlay_origin(0, 7);

        Text::print("File", {2, 1});
        Text::print("Edit", {8, 1});

        PLATFORM.set_tile(Layer::overlay, 0, 1, 84);

    }


    void draw_dock()
    {
        for (int x = 0; x < 24; ++x) {
            for (int y = 0; y < 3; ++y) {
                PLATFORM.set_tile(Layer::overlay, x + 3, y + 18, 83);
            }
        }
        PLATFORM.set_tile(Layer::overlay, 3, 18, 85); // corners
        PLATFORM.set_tile(Layer::overlay, 26, 18, 86);
    }


    DesktopOS() :
        mem_(allocate_dynamic<Mem>("desktop-gui", this))
    {
    }


    void enter(Scene&) override
    {
        PLATFORM.load_sprite_texture("spritesheet_os");

        PLATFORM.speaker().stop_music();
        PLATFORM.load_tile0_texture("wallpaper_flattened");
        for (int x = 0; x < 30; ++x) {
            for (int y = 0; y < 20; ++y) {
                PLATFORM.set_raw_tile(Layer::map_0, x, y, 32);
            }
        }
        __draw_image(1, 0, 1, 30, 16, Layer::map_0);
        PLATFORM.screen().schedule_fade(0);

        PLATFORM.fill_overlay(0);
        PLATFORM.load_overlay_texture("overlay_os");

        draw_menu_bar();
        draw_dock();

        PLATFORM.screen().set_view({});
        PLATFORM.screen().schedule_fade(0.5f);
        PLATFORM.screen().schedule_fade(0);

        cursor_ = {116.0_fixed, 76.0_fixed};

        auto spacing = 23.0_fixed;

        Vec2<Fixnum> icon_pos;
        icon_pos.x = 30.0_fixed;
        icon_pos.y = 139.0_fixed;
        mem_->dock_icons_.emplace_back(this, "Seeker", 13, icon_pos);
        icon_pos.x += spacing;
        mem_->dock_icons_.emplace_back(this, "Compass", 16, icon_pos);
        icon_pos.x += spacing + 1.0_fixed;
        mem_->dock_icons_.emplace_back(this, "Calculator", 17, icon_pos);
        icon_pos.x += spacing;
        mem_->dock_icons_.emplace_back(this, "SkyTunes", 15, icon_pos);
        icon_pos.x += spacing + 1.0_fixed;
        mem_->dock_icons_.emplace_back(this, "TextEdit", 18, icon_pos);
        icon_pos.x += spacing;
        mem_->dock_icons_.emplace_back(this, "Activity Monitor", 14, icon_pos);
        icon_pos.x += spacing;
        mem_->dock_icons_.emplace_back(this, "mGBA", 19, icon_pos);
        icon_pos.x += spacing;
        mem_->dock_icons_.emplace_back(this, "Calculator", 14, icon_pos);
    }


    void exit(Scene&) override
    {
    }


    ScenePtr update(Time delta) override
    {
        player().update(delta);

        if (player().key_pressed(Key::down)) {
            if (cursor_.y < 159.0_fixed) {
                cursor_.y += 1.3_fixed;
            }
        }
        if (player().key_pressed(Key::up)) {
            if (cursor_.y > 0.0_fixed) {
                cursor_.y -= 1.3_fixed;
            }
        }

        if (player().key_pressed(Key::right)) {
            if (cursor_.x < 239.0_fixed) {
                cursor_.x += 1.3_fixed;
            }
        }
        if (player().key_pressed(Key::left)) {
            if (cursor_.x > 0.0_fixed) {
                cursor_.x -= 1.3_fixed;
            }
        }

        HitBox cursor_hb;
        cursor_hb.dimension_ = {1, 1, 0, 0};
        cursor_hb.position_ = &cursor_;

        bool has_hover = false;
        pointer_ = false;
        for (auto& cl : mem_->clickables_) {
            if (cl->enabled() and cl->hitbox().overlapping(cursor_hb)) {
                if (cl->shows_pointer()) {
                    pointer_ = true;
                }
                cl->on_hover();
                has_hover = true;
            }
        }
        if (not has_hover) {
            if (hint_label_) {
                hint_label_.reset();
                repaint_windows();
            }
        }

        if (player().key_down(Key::action_1)) {
            click();
        }

        for (auto& ico : mem_->dock_icons_) {
            ico.update();
        }

        for (auto& win : mem_->windows_) {
            win->update();
        }

        return null_scene();
    }


    void click()
    {
        HitBox cursor_hb;
        cursor_hb.dimension_ = {1, 1, 0, 0};
        cursor_hb.position_ = &cursor_;

        for (auto& clickable : mem_->clickables_) {
            if (clickable->enabled() and clickable->hitbox().overlapping(cursor_hb)) {
                clickable->on_click();
                break;
            }
        }
    }


    void display() override
    {
        Sprite spr;
        spr.set_size(Sprite::Size::w16_h32);
        spr.set_texture_index(12);
        auto pos = cursor_;
        if (pointer_) {
            spr.set_texture_index(30);
            pos.x -= 4.0_fixed;
        }
        spr.set_position(pos);
        spr.set_priority(0);
        PLATFORM.screen().draw(spr);

        for (auto& ico : mem_->dock_icons_) {
            spr.set_texture_index(ico.icon_gfx());
            spr.set_position(ico.pos());
            PLATFORM.screen().draw(spr);
        }
    }


    Optional<Text> hint_label_;


    void focus_window(const char* name)
    {
        if (mem_->windows_.empty()) {
            return;
        }
        if (str_eq(mem_->windows_.back()->name(), name)) {
            mem_->windows_.back()->minimized_ = false;
            update_focus();
            repaint_windows();
            return;
        }

        for (auto& win : mem_->windows_) {
            if (str_eq(win->name(), name)) {
                std::swap(win, mem_->windows_.back());
                mem_->windows_.back()->minimized_ = false;
                update_focus();
                repaint_windows();
                return;
            }
        }
    }


    void minimize_window(const char* name)
    {
        for (auto& win : mem_->windows_) {
            if (str_eq(win->name(), name)) {
                win->minimized_ = true;
                std::swap(mem_->windows_[0], win);
                for (auto& win2 : reversed(mem_->windows_)) {
                    if (not win2->minimized_) {
                        focus_window(win2->name());
                        break;
                    }
                }
            }
        }
    }


    void repaint_windows()
    {
        for (int x = 0; x < 30; ++x) {
            for (int y = 2; y < 17; ++y) {
                PLATFORM.set_tile(Layer::overlay, x, y, 0);
            }
        }
        if (not mem_->windows_.empty()) {
            int i = mem_->windows_.size() - 1;
            while (i > -1) {
                if (not mem_->windows_[i]->minimized_) {
                    mem_->windows_[i]->repaint();
                    break;
                }
                --i;
            }

        }
    }


    void make_window(DockIcon* application)
    {
        if (str_eq(application->name(), "Compass")) {
            mem_->windows_.push_back(allocate_dynamic<ExplorerWindow>("os-window",
                                                                      this,
                                                                      application));
        } else if (str_eq(application->name(), "SkyTunes")) {
            mem_->windows_.push_back(allocate_dynamic<SkyTunesWindow>("os-window",
                                                                      this,
                                                                      application));
        } else if (str_eq(application->name(), "mGBA")) {
            mem_->windows_.push_back(allocate_dynamic<mGBAWindow>("os-window",
                                                                  this,
                                                                  application));
        } else {
            mem_->windows_.push_back(allocate_dynamic<Window>("os-window",
                                                              this,
                                                              application));
        }
        update_focus();
        repaint_windows();
    }


    void close_window(const char* name)
    {
        for (auto it = mem_->windows_.begin(); it not_eq mem_->windows_.end();) {
            if (str_eq(name, (*it)->name())) {
                it = mem_->windows_.erase(it);
            } else {
                ++it;
            }
        }
        update_focus();
        repaint_windows();
    }


    void update_focus()
    {
        if (mem_->windows_.empty()) {
            return;
        }
        for (auto& win : mem_->windows_) {
            win->set_focus(false);
        }
        mem_->windows_.back()->set_focus(true);
    }


private:
    Vec2<Fixnum> cursor_;

    class AppleMenu : public Clickable
    {
    public:
        AppleMenu(DesktopOS* os) : Clickable(os, {8, 8, 0, 0})
        {
        }
    };

    struct Mem {
        Buffer<DockIcon, 8> dock_icons_;
        Buffer<DynamicMemory<Window>, 8> windows_;
        Buffer<Clickable*, 50> clickables_;
        AppleMenu apple_menu_;

        Mem(DesktopOS* os) : apple_menu_(os)
        {
        }
    };

    DynamicMemory<Mem> mem_;
    bool pointer_ = false;
};



class DesktopBoot : public Scene
{
public:

    void enter(Scene& prev) override
    {
        PLATFORM.screen().schedule_fade(1);
        PLATFORM.speaker().stop_music();
    }


    ScenePtr update(Time delta)
    {
        if (intro_sleep_ < seconds(1)) {
            intro_sleep_ += delta;
            if (intro_sleep_ >= seconds(1)) {

            }
            return null_scene();
        }

        timer_ += delta;

        if (timer_ > seconds(1)) {
            return make_scene<DesktopOS>();
        }
        return null_scene();
    }


private:
    Time timer_;
    Time intro_sleep_;
};



}
