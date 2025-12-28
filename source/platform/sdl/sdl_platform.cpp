////////////////////////////////////////////////////////////////////////////////
//
// Copyright 2024 Evan Bowman
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the “Software”), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.  THE SOFTWARE IS PROVIDED
// “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
// LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.//
//
////////////////////////////////////////////////////////////////////////////////


#include "number/random.hpp"
#include "platform/flash_filesystem.hpp"
#include "platform/platform.hpp"
#include <SDL2/SDL.h>
#include <fstream>
#include <iostream>
#include <map>
#include <queue>
#include <sstream>
#include <thread>
#include <SDL2/SDL_image.h>



#ifdef _WIN32
#define PATH_DELIMITER "\\"
#else
#define PATH_DELIMITER "/"
#endif



enum class GlobalFlag {
    rtc_faulty,
    gbp_unlocked,
    multiplayer_connected,
    save_using_flash,
    glyph_mode,
    parallax_clouds,
    v_parallax,
    partial_palette_sync,
    palette_sync,
    sound_startup_monkeypatch,
    key_poll_called,
    watchdog_disabled,
    effect_window_mode,
    iris_effect_mode,
    skyland_custom_mgba,
    count
};


static Bitvector<static_cast<int>(GlobalFlag::count)> gflags;



bool console_running = false;



static void set_gflag(GlobalFlag f, bool val)
{
    gflags.set(static_cast<int>(f), val);
}


static bool get_gflag(GlobalFlag f)
{
    return gflags.get(static_cast<int>(f));
}



std::string resource_path();



void start(Platform&);



SDL_Window* window = nullptr;
static SDL_Renderer* renderer = nullptr;
bool sdl_running = true;
static bool is_fullscreen = false;
static int window_scale = 2;
static const int logical_width = 240;
static const int logical_height = 160;

static bool window_size_initialized = false;
static int last_window_width = 0;
static int last_window_height = 0;

static SDL_Texture* tile_recolor_buffer = nullptr;


static int circle_effect_radius = 0;
static int circle_effect_origin_x = 0;
static int circle_effect_origin_y = 0;



static const Platform::Extensions extensions{
    .overlay_circle_effect = [](int radius, int x, int y) {
        circle_effect_radius = radius;
        circle_effect_origin_x = x;
        circle_effect_origin_y = y;
    }
};



const Platform::Extensions& Platform::get_extensions()
{
    return extensions;
}



static int calculate_best_scale(int window_w, int window_h)
{
    // Calculate maximum scale that fits in both dimensions
    int scale_x = window_w / logical_width;
    int scale_y = window_h / logical_height;

    // Use the smaller scale to ensure it fits
    int scale = std::min(scale_x, scale_y);

    // Clamp to minimum of 1
    return std::max(1, scale);
}



static void constrain_window_to_scale()
{
    if (is_fullscreen) {
        return;  // Don't constrain in fullscreen
    }

    int current_w, current_h;
    SDL_GetWindowSize(window, &current_w, &current_h);

    // Calculate the best integer scale for current size
    int scale = calculate_best_scale(current_w, current_h);

    // Calculate the exact size for this scale
    int target_w = logical_width * scale;
    int target_h = logical_height * scale;

    // Check if we're closer to the next scale up
    int next_scale = scale + 1;
    int next_w = logical_width * next_scale;
    int next_h = logical_height * next_scale;

    // Calculate distance to current scale and next scale
    int dist_current = abs(current_w - target_w) + abs(current_h - target_h);
    int dist_next = abs(current_w - next_w) + abs(current_h - next_h);

    // Use whichever is closer
    if (dist_next < dist_current) {
        scale = next_scale;
        target_w = next_w;
        target_h = next_h;
    }

    // Only resize if different from current size
    if (current_w != target_w || current_h != target_h) {
        SDL_SetWindowSize(window, target_w, target_h);
        window_scale = scale;

        info(format("Window resized to %x scale (%x%)",
                   scale, target_w, target_h));
    }
}



static void update_viewport()
{
    int window_w, window_h;
    SDL_GetWindowSize(window, &window_w, &window_h);

    SDL_Rect viewport;

    if (is_fullscreen) {
        // In fullscreen, use integer scaling with letterboxing
        int scale = calculate_best_scale(window_w, window_h);
        int scaled_width = logical_width * scale;
        int scaled_height = logical_height * scale;

        // Center the viewport
        viewport.x = (window_w - scaled_width) / 2;
        viewport.y = (window_h - scaled_height) / 2;
        viewport.w = scaled_width;
        viewport.h = scaled_height;
    } else {
        // In windowed mode, use exact window size (already constrained)
        viewport.x = 0;
        viewport.y = 0;
        viewport.w = window_w;
        viewport.h = window_h;
    }

    SDL_RenderSetViewport(renderer, &viewport);
    SDL_RenderSetLogicalSize(renderer, logical_width, logical_height);

    // Use nearest neighbor (point) filtering for crisp pixels
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "nearest");
}



static void toggle_fullscreen()
{
    is_fullscreen = !is_fullscreen;

    if (is_fullscreen) {
        // Store current windowed size
        SDL_GetWindowSize(window, &last_window_width, &last_window_height);

        // Get desktop display mode for current display
        SDL_DisplayMode display_mode;
        int display_index = SDL_GetWindowDisplayIndex(window);
        SDL_GetDesktopDisplayMode(display_index, &display_mode);

        // On macOS, use native fullscreen for better behavior
#ifdef __APPLE__
        SDL_SetWindowFullscreen(window, SDL_WINDOW_FULLSCREEN_DESKTOP);
#else
        SDL_SetWindowFullscreen(window, SDL_WINDOW_FULLSCREEN_DESKTOP);
#endif

        info(format("Entered fullscreen mode: %x%",
                   display_mode.w, display_mode.h));
    } else {
        // Exit fullscreen
        SDL_SetWindowFullscreen(window, 0);

        // Restore to previous size if we have it
        if (last_window_width > 0 && last_window_height > 0) {
            SDL_SetWindowSize(window, last_window_width, last_window_height);
        } else {
            // Default to current scale
            SDL_SetWindowSize(window,
                            logical_width * window_scale,
                            logical_height * window_scale);
        }

         info("Exited fullscreen mode");
     }

    update_viewport();
}



static void handle_window_resize(int w, int h)
{
    // Ignore resize events in fullscreen (macOS sends these during transitions)
    if (is_fullscreen) {
        update_viewport();
        return;
    }

    // Constrain to integer scale
    constrain_window_to_scale();
    update_viewport();
}



static void cycle_window_scale(bool increase)
{
    if (is_fullscreen) {
        return;  // Don't change scale in fullscreen
    }

    if (increase) {
        window_scale = std::min(window_scale + 1, 8);  // Max 8x
    } else {
        window_scale = std::max(window_scale - 1, 1);  // Min 1x
    }

    int new_w = logical_width * window_scale;
    int new_h = logical_height * window_scale;
    SDL_SetWindowSize(window, new_w, new_h);

    update_viewport();
}



Platform* __platform__ = nullptr;



int main(int argc, char** argv)
{
    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        fprintf(stderr, "SDL failed to initialise: %s\n", SDL_GetError());
        return 1;
    }

    SDL_SetHint(SDL_HINT_VIDEO_HIGHDPI_DISABLED, "0");

    int initial_width = logical_width * window_scale;
    int initial_height = logical_height * window_scale;

    window = SDL_CreateWindow("Skyland",
                              SDL_WINDOWPOS_UNDEFINED,
                              SDL_WINDOWPOS_UNDEFINED,
                              initial_width,
                              initial_height,
                              SDL_WINDOW_SHOWN |
                              SDL_WINDOW_RESIZABLE |
                              SDL_WINDOW_ALLOW_HIGHDPI);

    if (window == NULL) {
        fprintf(stderr, "SDL window failed to initialise: %s\n", SDL_GetError());
        return 1;
    }

    // Set minimum window size to 1x scale
    SDL_SetWindowMinimumSize(window, logical_width, logical_height);

    // Create renderer instead of getting surface
    renderer = SDL_CreateRenderer(window, -1,
                                  SDL_RENDERER_ACCELERATED |
                                  SDL_RENDERER_PRESENTVSYNC);

    if (renderer == NULL) {
        fprintf(stderr, "SDL renderer failed to initialise: %s\n", SDL_GetError());
        return 1;
    }

    // // Enable integer scaling for crisp pixels
    SDL_RenderSetIntegerScale(renderer, SDL_TRUE);

    tile_recolor_buffer = SDL_CreateTexture(renderer,
                                            SDL_PIXELFORMAT_RGBA8888,
                                            SDL_TEXTUREACCESS_TARGET,
                                            16, 16);

    rng::critical_state = time(nullptr);

    Platform pf;

    __platform__ = &pf;
    start(pf);

    SDL_DestroyRenderer(renderer);
    SDL_DestroyWindow(window);
    SDL_Quit();
}



////////////////////////////////////////////////////////////////////////////////
// Misc Platform stuff...
////////////////////////////////////////////////////////////////////////////////


void Platform::restart()
{
    // ...
    while (1)
        ;
}



struct TileInfo {
    TileDesc tile_desc;
    u16 palette;  // Store the palette index
    ColorConstant text_fg_color_;
    ColorConstant text_bg_color_;
};



static std::map<Layer, std::map<std::pair<u16, u16>, TileInfo>> tile_layers_;



static const std::map<u16, SDL_Color> special_palettes = {
    {15, {0xef, 0x0d, 0x54, 255}},  // Red (custom_color(0xef0d54))
    {14, {0x10, 0x31, 0x63, 255}},  // Blue-black (custom_color(0x103163))
    {13, {0xff, 0xff, 0xff, 255}},  // White (silver_white)
};



TileDesc Platform::get_tile(Layer layer, u16 x, u16 y)
{
    if (layer == Layer::overlay) {
        x %= 32;
        y %= 32;
    }

    auto it = tile_layers_[layer].find({x, y});
    if (it != tile_layers_[layer].end()) {
        return it->second.tile_desc;
    }
    return 0;
}


static SDL_Color default_text_fg_color = {0xcd, 0xc3, 0xeb, 255};  // Initial fallback
static SDL_Color default_text_bg_color = {0x00, 0x00, 0x00, 255};  // Initial fallback



ColorConstant default_fg_color()
{
    u32 hex = (default_text_fg_color.r << 16) |
              (default_text_fg_color.g << 8) |
              default_text_fg_color.b;
    return (ColorConstant)hex;
}



bool is_glyph(TileDesc);



void Platform::clear_layer(Layer layer)
{
    if (auto found = tile_layers_.find(layer); found not_eq tile_layers_.end()) {
        tile_layers_.erase(found);
    }
}



void Platform::set_tile(Layer layer,
                        u16 x,
                        u16 y,
                        TileDesc val,
                        Optional<u16> palette)
{
    switch (layer) {
    case Layer::map_0_ext:
        set_raw_tile(Layer::map_0, x, y, 0);
        set_raw_tile(Layer::map_0, x + 1, y, 0);
        set_raw_tile(Layer::map_0, x, y + 1, 0);
        set_raw_tile(Layer::map_0, x + 1, y + 1, 0);
        tile_layers_[layer][{x, y}] = {val, palette.value_or(0)};
        break;

    case Layer::map_1_ext:
        set_raw_tile(Layer::map_1, x, y, 0);
        set_raw_tile(Layer::map_1, x + 1, y, 0);
        set_raw_tile(Layer::map_1, x, y + 1, 0);
        set_raw_tile(Layer::map_1, x + 1, y + 1, 0);
        tile_layers_[layer][{x, y}] = {val, palette.value_or(0)};
        break;

    case Layer::map_0:
    case Layer::map_1:
    case Layer::background:
        tile_layers_[layer][{x, y}] = {val, palette.value_or(0)};
        break;

    case Layer::overlay:
        x %= 32;
        y %= 32;
        ColorConstant fg_color = custom_color(0);
        if (is_glyph(val)) {
            fg_color = default_fg_color();
        }
        tile_layers_[layer][{x, y}] = {val, palette.value_or(0), fg_color};
        break;
    }
}



void Platform::Keyboard::rumble(bool enabled)
{
}



static std::map<SDL_Keycode, Key> keymap = {
    {SDLK_z, Key::action_2},      // Z for A button
    {SDLK_x, Key::action_1},      // X for B button
    {SDLK_RETURN, Key::start},
    {SDLK_RSHIFT, Key::select},
    {SDLK_RIGHT, Key::right},
    {SDLK_LEFT, Key::left},
    {SDLK_DOWN, Key::down},
    {SDLK_UP, Key::up},
    {SDLK_a, Key::alt_1},         // A for L button
    {SDLK_s, Key::alt_2},         // S for R button
};



void Platform::Keyboard::poll()
{
    std::copy(std::begin(states_), std::end(states_), std::begin(prev_));

    // Process all pending events
    SDL_Event e;
    while (SDL_PollEvent(&e)) {
        switch (e.type) {
        case SDL_QUIT:
            sdl_running = false;
            break;

         case SDL_WINDOWEVENT:
            switch (e.window.event) {
            case SDL_WINDOWEVENT_RESIZED:
            case SDL_WINDOWEVENT_SIZE_CHANGED:
                handle_window_resize(e.window.data1, e.window.data2);
                break;
            case SDL_WINDOWEVENT_FOCUS_GAINED:
                // On macOS, re-constrain when window gains focus
                // This helps with issues after fullscreen transitions
                if (!window_size_initialized) {
                    constrain_window_to_scale();
                    window_size_initialized = true;
                }
                break;
            }
            break;

        case SDL_KEYDOWN: {
            // F11 or Cmd+F (macOS) or Alt+Enter for fullscreen
            if (e.key.keysym.sym == SDLK_F11 ||
                (e.key.keysym.sym == SDLK_f && (e.key.keysym.mod & KMOD_GUI)) ||
                (e.key.keysym.sym == SDLK_RETURN && (e.key.keysym.mod & KMOD_ALT))) {
                toggle_fullscreen();
                break;  // Don't fall through
            }
            // Cmd/Ctrl + Plus/Minus to change window scale
            if ((e.key.keysym.mod & (KMOD_GUI | KMOD_CTRL))) {
                if (e.key.keysym.sym == SDLK_EQUALS || e.key.keysym.sym == SDLK_PLUS) {
                    cycle_window_scale(true);
                    break;
                } else if (e.key.keysym.sym == SDLK_MINUS) {
                    cycle_window_scale(false);
                    break;
                }
            }
            if (e.key.keysym.sym == SDLK_F11 ||
                (e.key.keysym.sym == SDLK_RETURN && (e.key.keysym.mod & KMOD_ALT))) {
                toggle_fullscreen();
            }

            auto it = keymap.find(e.key.keysym.sym);
            if (it != keymap.end()) {
                states_[int(it->second)] = true;
            }
            break;
        }

        case SDL_KEYUP: {
            auto it = keymap.find(e.key.keysym.sym);
            if (it != keymap.end()) {
                states_[int(it->second)] = false;
            }
            break;
        }
        }
    }
}


static std::map<std::string, std::string> files;



std::pair<const char*, u32> Platform::load_file(const char* folder,
                                                const char* filename) const
{
    const auto name = std::string(folder) + PATH_DELIMITER + filename;

    std::string path;
    for (char c : name) {
        if (c == '/') {
            path += PATH_DELIMITER;
        } else {
            path += c;
        }
    }

    auto found = files.find(path);
    if (found == files.end()) {
        std::fstream file(resource_path() + path);
        if (not file) {
            error(format("missing file %", (resource_path() + path).c_str()));
        }
        std::stringstream buffer;
        buffer << file.rdbuf();
        files[path] = buffer.str();
    } else {
        return {found->second.c_str(), found->second.length()};
    }

    found = files.find(path);
    return {found->second.c_str(), found->second.length()};
}



struct DynamicTextureMapping
{
    bool reserved_ = false;
    u16 spritesheet_offset_ = 0;  // Where in the spritesheet to get data from
} dynamic_texture_mappings[Platform::dynamic_texture_count];



static inline bool is_dynamic_texture_index(u16 texture_index)
{
    return texture_index < (Platform::dynamic_texture_count * 2);
}



static inline u8 get_dynamic_slot_for_index(u16 texture_index)
{
    // Convert texture index to slot number
    // Indices 0-1 -> slot 0, indices 2-3 -> slot 1, etc.
    return texture_index / 2;
}



void Platform::DynamicTexture::remap(u16 spritesheet_offset)
{
    auto& mapping = dynamic_texture_mappings[mapping_index_];
    mapping.spritesheet_offset_ = spritesheet_offset;

    // info(format("Dynamic texture slot % remapped to spritesheet offset %",
    //            mapping_index_, spritesheet_offset));
}



static ObjectPool<PooledRcControlBlock<Platform::DynamicTexture,
                                       Platform::dynamic_texture_count>,
                  Platform::dynamic_texture_count>
    dynamic_texture_pool("dynamic-texture-pool");



Optional<Platform::DynamicTexturePtr> Platform::make_dynamic_texture()
{
    auto finalizer =
        [](PooledRcControlBlock<DynamicTexture, dynamic_texture_count>* ctrl) {
            auto& mapping = dynamic_texture_mappings[ctrl->data_.mapping_index()];
            mapping.reserved_ = false;
            dynamic_texture_pool.free(ctrl);
        };

    for (u8 i = 0; i < dynamic_texture_count; ++i) {
        if (not dynamic_texture_mappings[i].reserved_) {
            auto dt = create_pooled_rc<DynamicTexture, dynamic_texture_count>(
                &dynamic_texture_pool, finalizer, i);
            if (dt) {
                dynamic_texture_mappings[i].reserved_ = true;
                return *dt;
            }
        }
    }

    return {};
}



static const char* const save_file_name = "save.dat";



int save_capacity = 32000;



u8 save_buffer[32000];



int Platform::save_capacity()
{
    return ::save_capacity;
}



void Platform::erase_save_sector()
{
    memset(save_buffer, 0xff, sizeof save_buffer);
}



bool Platform::write_save_data(const void* data, u32 length, u32 offset)
{
    memcpy(save_buffer + offset, data, length);

    std::ofstream out(save_file_name,
                      std::ios_base::out | std::ios_base::binary);

    out.write((const char*)save_buffer, ::save_capacity);

    return true;
}



bool Platform::read_save_data(void* buffer, u32 data_length, u32 offset)
{
    memcpy(buffer, save_buffer + offset, data_length);

    std::ifstream in(save_file_name, std::ios_base::in | std::ios_base::binary);

    if (!in) {
        return true;
    }

    in.read((char*)save_buffer, ::save_capacity);

    return true;
}



Platform::TilePixels Platform::extract_tile(Layer layer, u16 tile)
{
    return {};
}



Platform::EncodedTile Platform::encode_tile(u8 tile_data[16][16])
{
    // TODO...
    return {};
}



Platform& Platform::instance()
{
    return *__platform__;
}



void Platform::set_palette(Layer layer, u16 x, u16 y, u16 palette)
{
    auto it = tile_layers_[layer].find({x, y});
    if (it != tile_layers_[layer].end()) {
        it->second.palette = palette;
    }
}



u16 Platform::get_palette(Layer layer, u16 x, u16 y)
{
    auto it = tile_layers_[layer].find({x, y});
    if (it != tile_layers_[layer].end()) {
        return it->second.palette;
    }
    return 0;
}



void Platform::set_raw_tile(Layer layer, u16 x, u16 y, TileDesc val)
{
    set_tile(layer, x, y, val);// FIXME: leaves junk tiles everywhere...
}



void Platform::blit_t0_erase(u16 index)
{
}



void Platform::blit_t1_erase(u16 index)
{
}



void Platform::blit_t0_tile_to_texture(u16 from_index, u16 to_index, bool hard)
{
}



void Platform::blit_t1_tile_to_texture(u16 from_index, u16 to_index, bool hard)
{
}



#include <filesystem>



void Platform::walk_filesystem(
    Function<8 * sizeof(void*), void(const char* path)> callback)
{
    auto res_path = resource_path();
    using recursive_directory_iterator = std::filesystem::recursive_directory_iterator;
    for (const auto& dirent : recursive_directory_iterator(res_path)) {
        if (dirent.is_regular_file()) {
            auto full_path = dirent.path().string();
            // Remove resource_path() prefix to get relative path
            if (full_path.size() > res_path.size() &&
                full_path.substr(0, res_path.size()) == res_path) {
                auto relative_path = full_path.substr(res_path.size());
                // Remove leading path delimiter if present
                if (!relative_path.empty() &&
                    (relative_path[0] == '/' || relative_path[0] == '\\')) {
                    relative_path = relative_path.substr(1);
                }
                relative_path = "/" + relative_path;
                callback(relative_path.c_str());
            }
        }
    }
}



struct GlyphCacheEntry {
    std::string texture_name;
    u16 offset_in_texture;
    TileDesc vram_tile_index;
    bool in_use;
};

static std::vector<GlyphCacheEntry> glyph_cache;
static const int max_glyph_cache_size = 256;

static SDL_Texture* current_overlay_texture = nullptr;
static int overlay_texture_width = 0;
static int overlay_texture_height = 0;

// For dynamic glyph mapping, we'll extend the texture
static SDL_Surface* overlay_surface = nullptr;
static int overlay_base_tile_count = 0; // How many tiles in the original texture


static std::map<std::string, SDL_Surface*> charset_surfaces;
static std::map<std::string, SDL_Surface*> overlay_source_cache;



bool is_glyph(TileDesc td)
{
    return td >= overlay_base_tile_count;
}



static SDL_Surface* load_charset_surface(const char* texture_name)
{
    auto it = charset_surfaces.find(texture_name);
    if (it != charset_surfaces.end()) {
        return it->second;
    }

    std::string full_path = resource_path() + "images" + PATH_DELIMITER + texture_name + ".png";

    SDL_Surface* surface = IMG_Load(full_path.c_str());
    if (!surface) {
        error(format("Failed to load charset %: %", full_path.c_str(), IMG_GetError()));
        return nullptr;
    }

    // Set black as transparent
    Uint32 color_key = SDL_MapRGB(surface->format, 0, 0, 16);
    SDL_SetColorKey(surface, SDL_TRUE, color_key);

    charset_surfaces[texture_name] = surface;

    return surface;
}



static void copy_tile_to_surface(SDL_Surface* dst, SDL_Surface* src,
                                 int dst_tile_x, int dst_tile_y,
                                 int src_tile_x, int src_tile_y)
{
    SDL_Rect src_rect;
    src_rect.x = src_tile_x * 8;
    src_rect.y = src_tile_y * 8;
    src_rect.w = 8;
    src_rect.h = 8;

    SDL_Rect dst_rect;
    dst_rect.x = dst_tile_x * 8;
    dst_rect.y = dst_tile_y * 8;
    dst_rect.w = 8;
    dst_rect.h = 8;

    SDL_FillRect(dst, &dst_rect, 0x000000);
    SDL_BlitSurface(src, &src_rect, dst, &dst_rect);
}



static void expand_overlay_surface()
{
    if (!overlay_surface) {
        return;
    }

    int old_width = overlay_surface->w;
    int old_height = overlay_surface->h;

    // Expand vertically (add a new row of 8 pixel height)
    int new_height = old_height + 8;

    SDL_Surface* new_surface = SDL_CreateRGBSurface(
        0, old_width, new_height,
        overlay_surface->format->BitsPerPixel,
        overlay_surface->format->Rmask,
        overlay_surface->format->Gmask,
        overlay_surface->format->Bmask,
        overlay_surface->format->Amask);

    if (!new_surface) {
        error(format("Failed to expand overlay surface: %", SDL_GetError()));
        return;
    }

    // Copy old surface to new surface
    SDL_BlitSurface(overlay_surface, nullptr, new_surface, nullptr);

    // Set color key on new surface
    Uint32 color_key = SDL_MapRGB(new_surface->format, 0xFF, 0x00, 0xFF);
    SDL_SetColorKey(new_surface, SDL_TRUE, color_key);

    // Replace old surface
    SDL_FreeSurface(overlay_surface);
    overlay_surface = new_surface;

    // Recreate texture from expanded surface
    if (current_overlay_texture) {
        SDL_DestroyTexture(current_overlay_texture);
    }

    current_overlay_texture = SDL_CreateTextureFromSurface(renderer, overlay_surface);
    SDL_SetTextureBlendMode(current_overlay_texture, SDL_BLENDMODE_BLEND);

    overlay_texture_width = overlay_surface->w;
    overlay_texture_height = overlay_surface->h;
}



TileDesc Platform::map_glyph(const utf8::Codepoint& glyph,
                             const TextureMapping& mapping_info)
{
    if (not get_gflag(GlobalFlag::glyph_mode)) {
        return 495; // bad_glyph equivalent
    }

    // Check if this glyph is already mapped
    for (auto& entry : glyph_cache) {
        if (entry.in_use &&
            entry.texture_name == mapping_info.texture_name_ &&
            entry.offset_in_texture == mapping_info.offset_) {
            return entry.vram_tile_index;
        }
    }

    // Try to find an unused slot
    int slot_index = -1;
    for (size_t i = 0; i < glyph_cache.size(); ++i) {
        if (!glyph_cache[i].in_use) {
            slot_index = i;
            break;
        }
    }

    // If no free slot, create a new one (up to the limit)
    if (slot_index == -1) {
        if (glyph_cache.size() >= max_glyph_cache_size) {
            // Cache full - could implement LRU eviction here
            warning("Glyph cache full!");
            return 495; // bad_glyph
        }

        slot_index = glyph_cache.size();
        glyph_cache.push_back(GlyphCacheEntry{});
    }

    // Load the charset texture
    SDL_Surface* charset = load_charset_surface(mapping_info.texture_name_);
    if (!charset) {
        return 495; // bad_glyph
    }

    // Calculate the tile index in the expanded overlay texture
    // We append new glyphs after the base overlay tiles
    TileDesc new_tile_index = overlay_base_tile_count + slot_index;

    // Check if we need to expand the overlay surface
    int tiles_per_row = overlay_texture_width / 8;
    int required_tiles = new_tile_index + 1;
    int current_tiles = (overlay_texture_width / 8) * (overlay_texture_height / 8);

    while (current_tiles < required_tiles) {
        expand_overlay_surface();
        current_tiles = (overlay_texture_width / 8) * (overlay_texture_height / 8);
    }

    // Calculate position in overlay surface where this glyph will go
    int dst_tile_x = new_tile_index % tiles_per_row;
    int dst_tile_y = new_tile_index / tiles_per_row;

    // Calculate position in charset surface where the glyph is
    int charset_tiles_per_row = charset->w / 8;
    int src_tile_x = mapping_info.offset_ % charset_tiles_per_row;
    int src_tile_y = mapping_info.offset_ / charset_tiles_per_row;

    // Copy the 8x8 glyph from charset to overlay surface
    copy_tile_to_surface(overlay_surface, charset,
                        dst_tile_x, dst_tile_y,
                        src_tile_x, src_tile_y);

    // Recreate the texture from the modified surface
    if (current_overlay_texture) {
        SDL_DestroyTexture(current_overlay_texture);
    }
    current_overlay_texture = SDL_CreateTextureFromSurface(renderer, overlay_surface);
    SDL_SetTextureBlendMode(current_overlay_texture, SDL_BLENDMODE_BLEND);

    // Update cache entry
    glyph_cache[slot_index] = {
        mapping_info.texture_name_,
        mapping_info.offset_,
        new_tile_index,
        true
    };

    return new_tile_index;
}



void Platform::set_tile(u16 x, u16 y, TileDesc glyph, const FontColors& colors)
{
    // For SDL, we'll ignore custom colors for now and just use the glyph
    // Custom colors would require creating palette-swapped versions of glyphs
    set_tile(Layer::overlay, x, y, glyph);

    auto& val = tile_layers_[Layer::overlay][{x, y}];
    val.text_fg_color_ = colors.foreground_;
    val.text_bg_color_ = colors.background_;
}



void Platform::enable_glyph_mode(bool enabled)
{
    set_gflag(GlobalFlag::glyph_mode, enabled);

    if (enabled) {
        // Clear glyph cache
        for (auto& entry : glyph_cache) {
            entry.in_use = false;
        }
    }
}



void Platform::fill_overlay(u16 tile_desc)
{
    // When filling overlay, mark all dynamic glyphs as unused
    for (auto& entry : glyph_cache) {
        entry.in_use = false;
    }

    // Fill all 32x32 overlay tiles with the specified tile
    for (u16 y = 0; y < 32; ++y) {
        for (u16 x = 0; x < 32; ++x) {
            tile_layers_[Layer::overlay][{x, y}] = {tile_desc, 0};
        }
    }
}



// Clean up on shutdown (add to Platform destructor or cleanup)
void cleanup_charset_surfaces()
{
    for (auto& [name, surface] : charset_surfaces) {
        SDL_FreeSurface(surface);
    }
    charset_surfaces.clear();

    for (auto& [name, surface] : overlay_source_cache) {
        SDL_FreeSurface(surface);
    }
    overlay_source_cache.clear();

}



void Platform::override_priority(Layer layer, int priority)
{
    // TODO...
}



void Platform::overwrite_t0_tile(u16 index, const EncodedTile& t)
{
    // TODO...
}



void Platform::overwrite_t1_tile(u16 index, const EncodedTile& t)
{
    // TODO...
}



void Platform::overwrite_sprite_tile(u16 index, const EncodedTile& t)
{
    // TODO...
}



void Platform::overwrite_overlay_tile(u16 index, const EncodedTile& t)
{
}



#define STB_IMAGE_IMPLEMENTATION
#include "stb_image.h"


static Platform::Screen::Shader current_shader = passthrough_shader;
static int current_shader_arg = 0;


struct DynamicPalette {
    std::map<u32, u8> color_to_index;  // RGB value -> palette index
    SDL_Color colors[256];              // Up to 256 unique colors
    u8 count = 0;

    u8 add_color(const SDL_Color& c) {
        u32 key = (c.r << 16) | (c.g << 8) | c.b;

        auto it = color_to_index.find(key);
        if (it != color_to_index.end()) {
            return it->second;  // Already in palette
        }

        if (count >= 256) {
            warning("Palette overflow - reusing index 0");
            return 0;
        }

        u8 index = count++;
        color_to_index[key] = index;
        colors[index] = c;
        return index;
    }
};



static ColorConstant sdl_color_to_colorconstant(const SDL_Color& c) {
    return (ColorConstant)((c.r << 16) | (c.g << 8) | c.b);
}



SDL_Color color_to_sdl(ColorConstant k) {
    u32 hex = (u32)k;

    // Extract RGB from hex color
    u8 r = (hex >> 16) & 0xFF;
    u8 g = (hex >> 8) & 0xFF;
    u8 b = hex & 0xFF;

    SDL_Color result = {r, g, b, 255};
    return result;
}



struct PngPalette {
    SDL_Color colors[256];
    int count = 0;
    bool found = false;
};



static PngPalette extract_png_palette(const std::string& path)
{
    PngPalette result;

    std::ifstream file(path, std::ios::binary);
    if (!file) {
        error(format("Failed to open % for palette extraction", path.c_str()));
        return result;
    }

    // Read and verify PNG signature
    unsigned char signature[8];
    file.read((char*)signature, 8);

    // PNG signature: 137 80 78 71 13 10 26 10
    const unsigned char png_sig[8] = {137, 80, 78, 71, 13, 10, 26, 10};
    if (memcmp(signature, png_sig, 8) != 0) {
        error(format("% is not a valid PNG file", path.c_str()));
        return result;
    }

    // Read chunks until we find PLTE
    while (file.good()) {
        // Read chunk length (4 bytes, big-endian)
        unsigned char length_bytes[4];
        file.read((char*)length_bytes, 4);
        if (!file.good()) break;

        uint32_t chunk_length = (length_bytes[0] << 24) |
                                (length_bytes[1] << 16) |
                                (length_bytes[2] << 8) |
                                length_bytes[3];

        // Read chunk type (4 bytes)
        char chunk_type[5] = {0};
        file.read(chunk_type, 4);
        if (!file.good()) break;

        // Check if this is a PLTE chunk
        if (strncmp(chunk_type, "PLTE", 4) == 0) {
            // PLTE chunk found! Each entry is 3 bytes (RGB)
            result.count = chunk_length / 3;

            if (result.count > 256) {
                warning(format("PNG palette has % entries, clamping to 256", result.count));
                result.count = 256;
            }

            info(format("Found PNG palette with % colors", result.count));

            for (int i = 0; i < result.count; i++) {
                unsigned char rgb[3];
                file.read((char*)rgb, 3);
                result.colors[i].r = rgb[0];
                result.colors[i].g = rgb[1];
                result.colors[i].b = rgb[2];
                result.colors[i].a = 255;
            }

            result.found = true;
            return result;
        }

        // Skip this chunk's data and CRC (chunk_length + 4 bytes for CRC)
        file.seekg(chunk_length + 4, std::ios::cur);
    }

    // No PLTE chunk found - not an indexed image
    return result;
}



static SDL_Surface* load_png_with_stb(const std::string& path,
                                      const char* name,
                                      ShaderPalette shader_palette)
{
    // First, try to extract palette from PNG
    PngPalette original_palette = extract_png_palette(path);

    // Load the image with stb_image
    int width, height, channels;
    unsigned char* data = stbi_load(path.c_str(), &width, &height, &channels, 0);

    if (!data) {
        error(format("stb_image failed to load %: %", path.c_str(), stbi_failure_reason()));
        return nullptr;
    }

    info(format("Loaded % with stb_image: %x% pixels, % channels",
                name, width, height, channels));

    SDL_Surface* surface = nullptr;

    // If we found a palette, we need to apply the shader
    if (original_palette.found) {
        info(format("Applying shader to indexed image % (% palette colors)",
                   name, original_palette.count));

        // Apply shader to the original palette
        SDL_Color transformed_palette[256];
        for (int i = 0; i < original_palette.count; i++) {
            ColorConstant original = sdl_color_to_colorconstant(original_palette.colors[i]);
            ColorConstant transformed = current_shader(std::move(shader_palette),
                                                       std::move(original),
                                                       std::move(current_shader_arg),
                                                       std::move(i));
            transformed_palette[i] = color_to_sdl(transformed);
        }

        // Now we need to map the RGB values back to palette indices
        // Build a lookup map: RGB -> palette index
        std::map<u32, u8> rgb_to_index;
        for (int i = 0; i < original_palette.count; i++) {
            u32 key = (original_palette.colors[i].r << 16) |
                      (original_palette.colors[i].g << 8) |
                      original_palette.colors[i].b;
            rgb_to_index[key] = i;
        }

        // Create surface from stb_image data
        int bpp = channels * 8;
        SDL_Surface* temp_surface = SDL_CreateRGBSurfaceFrom(
            data, width, height, bpp, width * channels,
            0x000000FF, 0x0000FF00, 0x00FF0000,
            channels == 4 ? 0xFF000000 : 0);

        if (!temp_surface) {
            stbi_image_free(data);
            error(format("Failed to create surface: %", SDL_GetError()));
            return nullptr;
        }

        // Convert to RGBA32 for processing
        SDL_Surface* rgba_surface = SDL_ConvertSurfaceFormat(
            temp_surface, SDL_PIXELFORMAT_RGBA32, 0);
        SDL_FreeSurface(temp_surface);
        stbi_image_free(data);

        if (!rgba_surface) {
            error(format("Failed to convert to RGBA32: %", SDL_GetError()));
            return nullptr;
        }

        // Create output surface
        SDL_Surface* result_surface = SDL_CreateRGBSurface(
            0, width, height, 32,
            rgba_surface->format->Rmask,
            rgba_surface->format->Gmask,
            rgba_surface->format->Bmask,
            rgba_surface->format->Amask);

        if (!result_surface) {
            error(format("Failed to create result surface: %", SDL_GetError()));
            SDL_FreeSurface(rgba_surface);
            return nullptr;
        }

        // Apply shader transformation
        if (SDL_MUSTLOCK(rgba_surface)) SDL_LockSurface(rgba_surface);
        if (SDL_MUSTLOCK(result_surface)) SDL_LockSurface(result_surface);

        Uint32* src_pixels = (Uint32*)rgba_surface->pixels;
        Uint32* dst_pixels = (Uint32*)result_surface->pixels;
        int pixel_count = width * height;

        for (int i = 0; i < pixel_count; i++) {
            Uint32 pixel = src_pixels[i];
            SDL_Color c;
            SDL_GetRGBA(pixel, rgba_surface->format, &c.r, &c.g, &c.b, &c.a);

            // Handle transparency
            if (c.a == 0) {
                dst_pixels[i] = 0;
                continue;
            }

            // Look up original palette index
            u32 key = (c.r << 16) | (c.g << 8) | c.b;
            auto it = rgb_to_index.find(key);

            if (it != rgb_to_index.end()) {
                // Found palette index - use shader-transformed color
                u8 index = it->second;
                SDL_Color& transformed = transformed_palette[index];

                dst_pixels[i] = SDL_MapRGBA(result_surface->format,
                                            transformed.r,
                                            transformed.g,
                                            transformed.b,
                                            c.a);
            } else {
                // Not in palette? This shouldn't happen for indexed images
                // Just pass through
                dst_pixels[i] = pixel;
            }
        }

        if (SDL_MUSTLOCK(rgba_surface)) SDL_UnlockSurface(rgba_surface);
        if (SDL_MUSTLOCK(result_surface)) SDL_UnlockSurface(result_surface);

        SDL_FreeSurface(rgba_surface);

        // Set magenta as color key
        Uint32 magenta = SDL_MapRGB(result_surface->format, 0xFF, 0x00, 0xFF);
        SDL_SetColorKey(result_surface, SDL_TRUE, magenta);

        return result_surface;

    } else {
        // No palette - RGB/RGBA image, load without shader
        info(format("% is RGB/RGBA, loading without shader", name));

        int bpp = channels * 8;
        surface = SDL_CreateRGBSurfaceFrom(
            data, width, height, bpp, width * channels,
            0x000000FF, 0x0000FF00, 0x00FF0000,
            channels == 4 ? 0xFF000000 : 0);

        if (!surface) {
            stbi_image_free(data);
            error(format("Failed to create surface: %", SDL_GetError()));
            return nullptr;
        }

        SDL_Surface* copy = SDL_ConvertSurface(surface, surface->format, 0);
        SDL_FreeSurface(surface);
        stbi_image_free(data);

        if (copy) {
            Uint32 magenta = SDL_MapRGB(copy->format, 0xFF, 0x00, 0xFF);
            SDL_SetColorKey(copy, SDL_TRUE, magenta);
        }

        return copy;
    }
}



void load_metatiled_chunk(SDL_Surface* source_surface,
                          int meta_tile_size,
                          int tiles_per_meta,
                          int tiles_per_strip,
                          int strips_per_meta,
                          int copy_count,
                          TileDesc src,
                          TileDesc dst)
{
    const int tile_size = 8;
    const int tiles_per_row_src = source_surface->w / tile_size;
    const int meta_tiles_per_row = source_surface->w / meta_tile_size;

    // Number of meta-tiles to copy
    int num_meta_tiles = copy_count / tiles_per_meta;

    for (u16 i = 0; i < num_meta_tiles; ++i) {
        // src refers to the starting TILE index
        // Each meta-tile we want is tiles_per_meta apart
        TileDesc src_tile_index = src + (i * tiles_per_meta);

        // Convert tile index to meta-tile index
        int meta_tile_index = src_tile_index / tiles_per_meta;

        // Find which meta-tile block in the source texture
        int src_meta_x = meta_tile_index % meta_tiles_per_row;
        int src_meta_y = meta_tile_index / meta_tiles_per_row;

        // Base pixel position of this meta-tile block
        int base_src_x = src_meta_x * meta_tile_size;
        int base_src_y = src_meta_y * meta_tile_size;

        // Destination starts at dst + (i * tiles_per_meta)
        TileDesc dst_base = dst + (i * tiles_per_meta);

        // Copy horizontal strips
        for (int strip = 0; strip < strips_per_meta; ++strip) {
            // Copy tiles in this strip
            for (int tile_in_strip = 0; tile_in_strip < tiles_per_strip; ++tile_in_strip) {
                // Source: this 8x8 tile within the meta-tile block
                SDL_Rect src_rect;
                src_rect.x = base_src_x + (tile_in_strip * tile_size);
                src_rect.y = base_src_y + (strip * tile_size);
                src_rect.w = tile_size;
                src_rect.h = tile_size;

                // Destination: linear placement (always y=0)
                int linear_tile_index = strip * tiles_per_strip + tile_in_strip;
                TileDesc dst_tile = dst_base + linear_tile_index;

                SDL_Rect dst_rect;
                dst_rect.x = dst_tile * tile_size;
                dst_rect.y = 0;
                dst_rect.w = tile_size;
                dst_rect.h = tile_size;

                if (SDL_BlitSurface(source_surface, &src_rect, overlay_surface, &dst_rect) != 0) {
                    error(format("load_overlay_chunk: Failed to blit strip %/tile %: %",
                                 strip, tile_in_strip, SDL_GetError()));
                }
            }
        }
    }
}



static int FIXME_get_metatile_size(const char* texture_name)
{
    if (str_eq(texture_name, "character_art") or
        str_eq(texture_name, "appendix")) {
        return 4;
    } else if (str_eq(texture_name, "paint_icons")) {
        return 2;
    } else {
        return 1;
    }
}



StringBuffer<256> last_overlay_texture;
void Platform::load_overlay_chunk(TileDesc dst,
                                  TileDesc src,
                                  u16 count,
                                  const char* image_file)
{
    if (!overlay_surface) {
        error("load_overlay_chunk: overlay_surface not initialized");
        return;
    }

    SDL_Surface* source_surface = nullptr;
    std::string cache_key;

    if (image_file) {
        // Load the specified image file
        cache_key = image_file;

        // Check cache first
        auto it = overlay_source_cache.find(cache_key);
        if (it != overlay_source_cache.end()) {
            source_surface = it->second;
        } else {
            std::string full_path = resource_path() + "images" + PATH_DELIMITER +
                image_file + ".png";

            source_surface = load_png_with_stb(full_path, image_file, ShaderPalette::overlay);
            if (!source_surface) {
                error(format("load_overlay_chunk: Failed to load %", image_file));
                return;
            }

            // Cache it
            overlay_source_cache[cache_key] = source_surface;
        }
    } else {
        // Use current overlay texture - need to reload the full uncropped version
        if (last_overlay_texture.empty()) {
            error("load_overlay_chunk: No current overlay texture");
            return;
        }

        cache_key = last_overlay_texture.c_str();

        // Check cache first
        auto it = overlay_source_cache.find(cache_key);
        if (it != overlay_source_cache.end()) {
            source_surface = it->second;
        } else {
            std::string full_path = resource_path() + "images" + PATH_DELIMITER +
                cache_key + ".png";

            source_surface = load_png_with_stb(full_path, cache_key.c_str(), ShaderPalette::overlay);
            if (!source_surface) {
                error(format("load_overlay_chunk: Failed to reload current texture %",
                             cache_key.c_str()));
                return;
            }

            // Cache it
            overlay_source_cache[cache_key] = source_surface;
        }
    }

    const int tile_size = 8;
    const int tiles_per_row_dst = overlay_surface->w / tile_size;

    auto metatile_size = FIXME_get_metatile_size(image_file ? image_file : "");

    if (metatile_size > 1) {
        // Meta-tile mode: source has 32x32 pixel blocks, each containing 16 8x8 tiles
        // src is a TILE index (8x8), not a meta-tile index
        // Each 32x32 meta-tile = 16 8x8 tiles
        load_metatiled_chunk(source_surface,
                             metatile_size * 8,
                             metatile_size * metatile_size,
                             metatile_size,
                             metatile_size,
                             count,
                             src,
                             dst);
    } else {
        // Regular 8x8 tile mode
        const int tiles_per_row_src = source_surface->w / tile_size;

        for (u16 i = 0; i < count; ++i) {
            TileDesc src_tile = src + i;
            TileDesc dst_tile = dst + i;

            // Calculate source tile coordinates
            int src_tile_x = src_tile % tiles_per_row_src;
            int src_tile_y = src_tile / tiles_per_row_src;

            // Calculate destination tile coordinates
            int dst_tile_x = dst_tile % tiles_per_row_dst;
            int dst_tile_y = dst_tile / tiles_per_row_dst;

            SDL_Rect src_rect;
            src_rect.x = src_tile_x * tile_size;
            src_rect.y = src_tile_y * tile_size;
            src_rect.w = tile_size;
            src_rect.h = tile_size;

            SDL_Rect dst_rect;
            dst_rect.x = dst_tile_x * tile_size;
            dst_rect.y = dst_tile_y * tile_size;
            dst_rect.w = tile_size;
            dst_rect.h = tile_size;

            // Blit the tile
            if (SDL_BlitSurface(source_surface, &src_rect, overlay_surface, &dst_rect) != 0) {
                error(format("load_overlay_chunk: Failed to blit tile %: %",
                            i, SDL_GetError()));
            }
        }
    }


    // Recreate the texture from the modified surface
    if (current_overlay_texture) {
        SDL_DestroyTexture(current_overlay_texture);
    }

    current_overlay_texture = SDL_CreateTextureFromSurface(renderer, overlay_surface);
    if (!current_overlay_texture) {
        error(format("load_overlay_chunk: Failed to recreate texture: %",
                     SDL_GetError()));
        return;
    }

    SDL_SetTextureBlendMode(current_overlay_texture, SDL_BLENDMODE_BLEND);
}



static SDL_Texture* tile0_texture = nullptr;
static int tile0_texture_width = 0;
static int tile0_texture_height = 0;

static SDL_Texture* tile1_texture = nullptr;
static int tile1_texture_width = 0;
static int tile1_texture_height = 0;

static Vec2<s32> tile0_scroll;
static Vec2<s32> tile1_scroll;
Vec2<Float> overlay_origin;

static bool tile0_index_zero_is_transparent = true;
static bool tile1_index_zero_is_transparent = true;
static bool overlay_index_zero_is_transparent = true;



static bool is_tile_transparent(SDL_Surface* surface, int tile_x, int tile_y, int tile_size = 8)
{
    if (!surface) {
        return true;
    }

    if (SDL_MUSTLOCK(surface)) {
        if (SDL_LockSurface(surface) != 0) {
            error(format("Failed to lock surface: %", SDL_GetError()));
            return true;
        }
    }

    bool is_transparent = true;
    Uint8* pixels = (Uint8*)surface->pixels;
    int pitch = surface->pitch;
    int bpp = surface->format->BytesPerPixel;

    for (int y = 0; y < tile_size && is_transparent; ++y) {
        for (int x = 0; x < tile_size && is_transparent; ++x) {
            int pixel_x = tile_x * tile_size + x;
            int pixel_y = tile_y * tile_size + y;

            Uint8* pixel_ptr = pixels + pixel_y * pitch + pixel_x * bpp;
            Uint32 pixel_value = *(Uint32*)pixel_ptr;

            Uint8 r, g, b, a;
            SDL_GetRGBA(pixel_value, surface->format, &r, &g, &b, &a);

            // Check if pixel is not transparent (alpha > 0)
            if (r == 255 and g == 0 and b == 255) {

            } else {
                if (a > 0) {
                    is_transparent = false;
                }
            }

        }
    }

    if (SDL_MUSTLOCK(surface)) {
        SDL_UnlockSurface(surface);
    }

    return is_transparent;
}



void Platform::set_scroll(Layer layer, u16 x, u16 y)
{
    s16 xx = x;
    s16 yy = y;
    xx %= 512; // To emulate gba scroll wrapping.
    switch (layer) {
    case Layer::map_0_ext:
    case Layer::map_0:
        tile0_scroll = {(s32)xx, (s32)yy};
        break;

    case Layer::map_1_ext:
    case Layer::map_1:
        tile1_scroll = {(s32)xx, (s32)yy};
        break;

    case Layer::overlay:
        overlay_origin = {(Float)xx, (Float)yy};
        break;

    default:
        break;
    }
}



Vec2<u16> Platform::get_scroll(Layer layer)
{
    switch (layer) {
    case Layer::map_0_ext:
    case Layer::map_0:
        return {(u16)tile0_scroll.x, (u16)tile0_scroll.y};

    case Layer::map_1_ext:
    case Layer::map_1:
        return {(u16)tile1_scroll.x, (u16)tile1_scroll.y};

    default:
        return {};
    }
}



static SDL_Rect get_tile_source_rect_16x16(TileDesc tile_index, int texture_width)
{
    SDL_Rect src;

    const int tile_size = 16;
    const int tiles_per_row = texture_width / tile_size;

    int tile_x = tile_index % tiles_per_row;
    int tile_y = 0;

    src.x = tile_x * tile_size;
    src.y = tile_y * tile_size;
    src.w = tile_size;
    src.h = tile_size;

    return src;
}



static SDL_Rect get_tile_source_rect_8x8(TileDesc tile_index, int texture_width)
{
    SDL_Rect src;

    const int tile_size = 8;
    const int tiles_per_row = texture_width / tile_size;

    int tile_x = tile_index % tiles_per_row;
    int tile_y = tile_index / tiles_per_row;

    src.x = tile_x * tile_size;
    src.y = tile_y * tile_size;
    src.w = tile_size;
    src.h = tile_size;

    return src;
}



static std::string extract_texture_name(const char* path_or_name)
{
    std::string input(path_or_name);

    // Find the last path separator (/ or \)
    size_t last_slash = input.find_last_of("/\\");
    std::string filename = (last_slash != std::string::npos)
        ? input.substr(last_slash + 1)
        : input;

    // Remove any extension (.img.bin, .pal.bin, .png, etc.)
    size_t first_dot = filename.find('.');
    if (first_dot != std::string::npos) {
        filename = filename.substr(0, first_dot);
    }

    return filename;
}



void Platform::load_tile0_texture(const char* name_or_path)
{
    auto name = extract_texture_name(name_or_path);

    if (tile0_texture) {
        SDL_DestroyTexture(tile0_texture);
        tile0_texture = nullptr;
    }

    std::string full_path = resource_path() + "images" + PATH_DELIMITER + name + ".png";

    SDL_Surface* surface = load_png_with_stb(full_path, name.c_str(), ShaderPalette::tile0);
    if (!surface) {
        return;
    }

    // std::string debug_path = "debug_loaded_" + name + ".bmp";
    // if (SDL_SaveBMP(surface, debug_path.c_str()) == 0) {
    //     info(format("Saved debug image to %", debug_path.c_str()));
    // } else {
    //     error(format("Failed to save debug image: %", SDL_GetError()));
    // }

    tile0_index_zero_is_transparent = is_tile_transparent(surface, 0, 0, 0);

    tile0_texture = SDL_CreateTextureFromSurface(renderer, surface);
    if (!tile0_texture) {
        error(format("Failed to create tile0 texture from %: %", full_path.c_str(), SDL_GetError()));
        SDL_FreeSurface(surface);
        return;
    }

    SDL_SetTextureBlendMode(tile0_texture, SDL_BLENDMODE_BLEND);

    tile0_texture_width = surface->w;
    tile0_texture_height = surface->h;

    SDL_FreeSurface(surface);
}



void Platform::load_tile1_texture(const char* name_or_path)
{
    info(name_or_path);
    auto name = extract_texture_name(name_or_path);

    if (tile1_texture) {
        SDL_DestroyTexture(tile1_texture);
        tile1_texture = nullptr;
    }

    std::string full_path = resource_path() + "images" + PATH_DELIMITER + name + ".png";

    SDL_Surface* surface = load_png_with_stb(full_path, name.c_str(), ShaderPalette::tile1);
    if (!surface) {
        return;
    }

    tile1_index_zero_is_transparent = is_tile_transparent(surface, 0, 0, 0);

    tile1_texture = SDL_CreateTextureFromSurface(renderer, surface);
    if (!tile1_texture) {
        error(format("Failed to create tile1 texture from %: %", full_path.c_str(), SDL_GetError()));
        SDL_FreeSurface(surface);
        return;
    }

    SDL_SetTextureBlendMode(tile1_texture, SDL_BLENDMODE_BLEND);

    tile1_texture_width = surface->w;
    tile1_texture_height = surface->h;

    info(format("Loaded tile1 texture %: %x% pixels", name.c_str(),
               tile1_texture_width, tile1_texture_height));

    SDL_FreeSurface(surface);
}



TileDesc Platform::map_tile0_chunk(TileDesc src)
{
    if (src > 0) {
        return src + 127;
    }
    return 0;
}



TileDesc Platform::map_tile1_chunk(TileDesc src)
{
    if (src > 0) {
        return src + 127;
    }
    return 0;
}



static float wrap_y(float y)
{
    if (y > 160) {
        // FIXME: gba automatically wraps tile layers, and I happened to be
        // displaying a wrapped region of a tile layer on the gba. I could fix
        // it, but it's easier just to do this:
        y -= 508;
    }

    return y;
}



void Platform::set_overlay_origin(Float x, Float y)
{
    while (y < -256) {
        y += 256;
    }
    overlay_origin = {x, wrap_y(y)};
}



static SDL_Texture* current_sprite_texture = nullptr;
static int sprite_texture_width = 0;
static int sprite_texture_height = 0;



void Platform::load_sprite_texture(const char* name)
{
    // Clean up old texture if it exists
    if (current_sprite_texture) {
        SDL_DestroyTexture(current_sprite_texture);
        current_sprite_texture = nullptr;
    }

    std::string full_path = resource_path() + "images" + PATH_DELIMITER + name + ".png";

    SDL_Surface* surface = load_png_with_stb(full_path, name, ShaderPalette::spritesheet);
    if (!surface) {
        error(format("Failed to load image %: %", full_path.c_str(), IMG_GetError()));
        return;
    }

    // Convert indexed color to RGBA32 if needed
    if (surface->format->palette != nullptr) {
        SDL_Surface* converted = SDL_ConvertSurfaceFormat(surface,
                                                           SDL_PIXELFORMAT_RGBA32,
                                                           0);
        SDL_FreeSurface(surface);
        if (!converted) {
            error(format("Failed to convert sprite surface: %", SDL_GetError()));
            return;
        }
        surface = converted;
    }

    // Set magenta (0xFF00FF) as the transparent color key
    Uint32 color_key = SDL_MapRGB(surface->format, 0xFF, 0x00, 0xFF);
    SDL_SetColorKey(surface, SDL_TRUE, color_key);

    current_sprite_texture = SDL_CreateTextureFromSurface(renderer, surface);
    if (!current_sprite_texture) {
        error(format("Failed to create texture from %: %", full_path.c_str(), SDL_GetError()));
        SDL_FreeSurface(surface);
        return;
    }

    // Enable alpha blending on the texture
    SDL_SetTextureBlendMode(current_sprite_texture, SDL_BLENDMODE_BLEND);

    sprite_texture_width = surface->w;
    sprite_texture_height = surface->h;

    SDL_FreeSurface(surface);
}


void Platform::load_background_texture(const char* name)
{
}



void Platform::clear_tile0_mappings()
{
}



void Platform::clear_tile1_mappings()
{
}



void Platform::fatal(const char* msg)
{
    std::cerr << msg << std::endl;
    exit(1);
}



static void extract_text_colors_from_overlay()
{
    if (!overlay_surface) {
        warning("extract_text_colors_from_overlay: overlay_surface not initialized");
        return;
    }

    // Font color index tile is at index 81 (matches GBA)
    const int font_color_index_tile = 81;
    const int tile_size = 8;
    const int tiles_per_row = overlay_surface->w / tile_size;

    int tile_x = font_color_index_tile % tiles_per_row;
    int tile_y = font_color_index_tile / tiles_per_row;

    int pixel_x = tile_x * tile_size;
    int pixel_y = tile_y * tile_size;

    // Lock surface for pixel access
    if (SDL_MUSTLOCK(overlay_surface)) {
        if (SDL_LockSurface(overlay_surface) != 0) {
            error(format("Failed to lock surface: %", SDL_GetError()));
            return;
        }
    }

    // Get the first two pixels (indices 0 and 1) which contain fg and bg color info
    Uint8* pixels = (Uint8*)overlay_surface->pixels;
    int pitch = overlay_surface->pitch;
    Uint32 pixel_format = overlay_surface->format->format;

    // Get pixel at (pixel_x, pixel_y) - foreground color
    Uint8* fg_pixel = pixels + pixel_y * pitch + pixel_x * overlay_surface->format->BytesPerPixel;
    Uint32 fg_pixel_value = *(Uint32*)fg_pixel;

    // Get pixel at (pixel_x + 1, pixel_y) - background color
    Uint8* bg_pixel = pixels + pixel_y * pitch + (pixel_x + 1) * overlay_surface->format->BytesPerPixel;
    Uint32 bg_pixel_value = *(Uint32*)bg_pixel;

    // Convert to SDL_Color
    SDL_GetRGBA(fg_pixel_value, overlay_surface->format,
                &default_text_fg_color.r, &default_text_fg_color.g,
                &default_text_fg_color.b, &default_text_fg_color.a);

    SDL_GetRGBA(bg_pixel_value, overlay_surface->format,
                &default_text_bg_color.r, &default_text_bg_color.g,
                &default_text_bg_color.b, &default_text_bg_color.a);

    if (SDL_MUSTLOCK(overlay_surface)) {
        SDL_UnlockSurface(overlay_surface);
    }

}



bool Platform::load_overlay_texture(const char* name)
{
    if (name == last_overlay_texture) {
        return true;
    }
    // Clean up old texture/surface if they exist
    if (current_overlay_texture) {
        SDL_DestroyTexture(current_overlay_texture);
        current_overlay_texture = nullptr;
    }
    if (overlay_surface) {
        SDL_FreeSurface(overlay_surface);
        overlay_surface = nullptr;
    }

    // Clear glyph cache
    for (auto& entry : glyph_cache) {
        entry.in_use = false;
    }

    std::string full_path = resource_path() + "images" + PATH_DELIMITER + name + ".png";

    SDL_Surface* loaded_surface = IMG_Load(full_path.c_str());
    if (!loaded_surface) {
        error(format("Failed to load overlay %: %", full_path.c_str(), IMG_GetError()));
        return false;
    }

    // CHECK DIMENSIONS BEFORE PROCEEDING

    if (loaded_surface->w == 0 || loaded_surface->h == 0) {
        error(format("Overlay % has invalid dimensions: %x%",
                     name, loaded_surface->w, loaded_surface->h));
        SDL_FreeSurface(loaded_surface);
        return false;
    }

    // Convert indexed color to RGBA32 if needed
    SDL_Surface* converted_surface = loaded_surface;
    if (loaded_surface->format->palette != nullptr) {
        converted_surface = SDL_ConvertSurfaceFormat(loaded_surface,
                                                      SDL_PIXELFORMAT_RGBA32,
                                                      0);
        SDL_FreeSurface(loaded_surface);
        if (!converted_surface) {
            error(format("Failed to convert surface: %", SDL_GetError()));
            return false;
        }
        loaded_surface = converted_surface;
    }

    // NOW set color key on the RGBA surface
    Uint32 color_key = SDL_MapRGB(loaded_surface->format, 0xFF, 0x00, 0xFF);
    SDL_SetColorKey(loaded_surface, SDL_TRUE, color_key);

    // GBA limitation: only load first 4032 pixels in X direction
    const int max_width = 4032;
    int clamped_width = std::min(loaded_surface->w, max_width);
    int clamped_height = loaded_surface->h;

    // Create a new surface with clamped dimensions
    overlay_surface = SDL_CreateRGBSurface(
        0, clamped_width, clamped_height,
        loaded_surface->format->BitsPerPixel,
        loaded_surface->format->Rmask,
        loaded_surface->format->Gmask,
        loaded_surface->format->Bmask,
        loaded_surface->format->Amask);

    if (!overlay_surface) {
        error(format("Failed to create clamped overlay surface: %", SDL_GetError()));
        SDL_FreeSurface(loaded_surface);
        return false;
    }

    // Copy the clamped portion
    SDL_Rect src_rect = {0, 0, clamped_width, clamped_height};
    SDL_BlitSurface(loaded_surface, &src_rect, overlay_surface, nullptr);

    SDL_FreeSurface(loaded_surface);

    // Extract text colors from tile 81 BEFORE creating the texture
    extract_text_colors_from_overlay();

    overlay_index_zero_is_transparent = is_tile_transparent(overlay_surface, 0, 0, 8);

    current_overlay_texture = SDL_CreateTextureFromSurface(renderer, overlay_surface);
    if (!current_overlay_texture) {
        error(format("Failed to create overlay texture from %: %", full_path.c_str(), SDL_GetError()));
        SDL_FreeSurface(overlay_surface);
        overlay_surface = nullptr;
        return false;
    }

    SDL_SetTextureBlendMode(current_overlay_texture, SDL_BLENDMODE_BLEND);

    overlay_texture_width = clamped_width;
    overlay_texture_height = clamped_height;

    // Calculate how many 8x8 tiles fit in the original texture
    overlay_base_tile_count = (overlay_texture_width / 8) * (overlay_texture_height / 8);

    last_overlay_texture = name;

    return true;
}



void Platform::on_unrecoverrable_error(UnrecoverrableErrorCallback callback)
{
    // ...
}



void Platform::sleep(u32 frames)
{
    const auto amount =
        frames * (std::chrono::duration_cast<std::chrono::microseconds>(
                      std::chrono::seconds(1)) /
                  60);

    std::this_thread::sleep_for(amount);
}



////////////////////////////////////////////////////////////////////////////////
// DeltaClock
////////////////////////////////////////////////////////////////////////////////



Platform::DeltaClock::DeltaClock()
{
}



std::chrono::time_point clk_prev = std::chrono::high_resolution_clock::now();
std::chrono::time_point clk_startup = std::chrono::high_resolution_clock::now();



Platform::DeltaClock::TimePoint Platform::DeltaClock::sample() const
{
    namespace chrono = std::chrono;
    auto clk = chrono::high_resolution_clock::now();
    return chrono::duration_cast<chrono::microseconds>(clk - clk_startup)
        .count();
}



static Microseconds last_delta = 1;



Microseconds Platform::DeltaClock::last_delta() const
{
    return ::last_delta;
}



Microseconds Platform::DeltaClock::reset()
{
    namespace chrono = std::chrono;

    auto clk = chrono::high_resolution_clock::now();
    auto elapsed = chrono::duration_cast<chrono::microseconds>(clk - clk_prev);

    clk_prev = clk;

    ::last_delta = elapsed.count();
    return elapsed.count();
}



Platform::DeltaClock::~DeltaClock()
{
}



////////////////////////////////////////////////////////////////////////////////
// Shaders
////////////////////////////////////////////////////////////////////////////////



ColorConstant
passthrough_shader(ShaderPalette palette, ColorConstant k, int arg, int index)
{
    return k;
}



ColorConstant
contrast_shader(ShaderPalette palette, ColorConstant k, int arg, int index)
{
    return k;
}



ColorConstant grayscale_shader(int palette, ColorConstant k, int arg)
{
    return k; // TODO
}



ColorConstant contrast_shader(int palette, ColorConstant k, int arg)
{
    return k; // TODO
}



ColorConstant passthrough_shader(int palette, ColorConstant k, int arg)
{
    return k;
}



////////////////////////////////////////////////////////////////////////////////
// NetworkPeer
////////////////////////////////////////////////////////////////////////////////



Platform::NetworkPeer::NetworkPeer() : impl_(nullptr)
{
}


void Platform::NetworkPeer::disconnect()
{
}


bool Platform::NetworkPeer::is_host() const
{
    return true;
}


void Platform::NetworkPeer::listen()
{
    return;
}


void Platform::NetworkPeer::connect(const char* peer)
{
    return;
}


bool Platform::NetworkPeer::is_connected() const
{
    return false; // TODO
}


bool Platform::NetworkPeer::send_message(const Message& message)
{
    return true; // TODO
}


static std::vector<u8> receive_buffer;


void Platform::NetworkPeer::update()
{
}


Optional<Platform::NetworkPeer::Message> Platform::NetworkPeer::poll_message()
{
    return {};
}


void Platform::NetworkPeer::poll_consume(u32 length)
{
}


Platform::NetworkPeer::~NetworkPeer()
{
}


Platform::NetworkPeer::Interface Platform::NetworkPeer::interface() const
{
    return Interface::internet;
}


int Platform::NetworkPeer::send_queue_size() const
{
    return 100000;
}



////////////////////////////////////////////////////////////////////////////////
// RemoteConsole
////////////////////////////////////////////////////////////////////////////////



std::mutex console_rcv_mutex;
std::string console_rcv_string;

std::mutex console_send_mutex;
std::queue<std::string> console_send_queue;



void console_thread_main()
{
    while (sdl_running) {
        std::string text;

        if (std::getline(std::cin, text)) {
            std::lock_guard<std::mutex> lock(console_rcv_mutex);
            std::swap(console_rcv_string, text);
        }
    }
    console_running = false;
}



void Platform::RemoteConsole::start()
{
    std::thread console_thread(console_thread_main);
    console_running = true;
    console_thread.detach();
}



auto Platform::RemoteConsole::readline() -> Optional<Line>
{
    std::lock_guard<std::mutex> lock(console_rcv_mutex);
    if (not console_rcv_string.empty()) {
        Line ret = console_rcv_string.c_str();
        console_rcv_string.clear();
        return ret;
    }

    return {};
}



bool Platform::RemoteConsole::printline(const char* text, const char* prompt)
{
    std::cout << text << "\n" << prompt << std::flush;
    return false;
}



static const char* const logfile_name = "logfile.txt";
static std::ofstream logfile_out(logfile_name);


static Severity log_threshold;


Optional<Vector<char>> log_data_;


void Platform::Logger::clear()
{
    log_data_.reset();
}


void Platform::Logger::set_threshold(Severity severity)
{
    log_threshold = severity;
}


void Platform::Logger::flush()
{
    if (not log_data_) {
        return;
    }

    flash_filesystem::store_file_data_binary("/log.txt", *log_data_);

    log_data_.reset();
}



void Platform::Logger::log(Severity level, const char* msg)
{
    if (::__platform__ == nullptr) {
        return;
    }

    ScratchBuffer::Tag t = "syslog_data";

    if (not log_data_) {
        log_data_.emplace(t);
    }

    while (*msg not_eq '\0') {
        log_data_->push_back(*msg, t);
        std::cout << *msg;
        ++msg;
    }

    log_data_->push_back('\n', t);
    std::cout << '\n';
    if (console_running) {
        std::cout << "> ";
    }
}



Platform::Logger::Logger()
{
}



Vector<char>* Platform::Logger::data()
{
    if (log_data_) {
        return &*log_data_;
    }

    return nullptr;
}



////////////////////////////////////////////////////////////////////////////////
// Screen
////////////////////////////////////////////////////////////////////////////////



Platform::Screen::Screen() : userdata_(nullptr)
{
}



void Platform::Screen::set_shader(Shader shader)
{
    current_shader = shader;
}



void Platform::Screen::set_shader_argument(int arg)
{
    current_shader_arg = arg;
}



struct SpriteDrawInfo {
    Vec2<s32> position;
    Sprite::Size size;
    ColorConstant color;
    bool visible;
    u16 texture_index;
    Vec2<bool> flip;
    double rotation;
    u8 priority;
    Sprite::Alpha alpha;
    u8 mix_amount;
    Vec2<double> scale;
};



static std::vector<SpriteDrawInfo> sprite_draw_list;



void Platform::Screen::draw_batch(TextureIndex t,
                                  const Buffer<Vec2<s32>, 64>& coords,
                                  const SpriteBatchOptions& opts)
{
    auto view_center = get_view().int_center().cast<s32>();

    for (const auto& coord : coords) {
        Vec2<s32> pos = coord;

        if (!opts.position_absolute_) {
            pos = pos - view_center;
        }

        pos.y = wrap_y(pos.y);

        sprite_draw_list.push_back({
            pos,
            opts.sz_,
            ColorConstant::null,  // No color mixing
            true,                 // visible
            t,                    // texture_index
            {false, false},       // flip
            0.0,                  // rotation
            1,                    // priority
            opts.alpha_,
            0                     // mix_amount
        });
    }
}



static SDL_Rect get_sprite_source_rect(u16 texture_index, Sprite::Size size)
{
    SDL_Rect src;

    // Check if this texture index is in the dynamic range and has been remapped
    if (is_dynamic_texture_index(texture_index)) {
        u8 slot = get_dynamic_slot_for_index(texture_index);
        auto& mapping = dynamic_texture_mappings[slot];

        if (mapping.reserved_) {
            // Use the remapped spritesheet offset instead of the texture_index
            texture_index = mapping.spritesheet_offset_;
        }
        // If not reserved, just use the original texture_index (shows default position)
    }

    // Normal sprite calculation using the (possibly remapped) texture_index
    const int meta_tile_width = 16;
    const int meta_tile_height = 32;
    int tiles_per_row = sprite_texture_width / meta_tile_width;
    int meta_tile_x = texture_index % tiles_per_row;
    int meta_tile_y = texture_index / tiles_per_row;
    int base_x = meta_tile_x * meta_tile_width;
    int base_y = meta_tile_y * meta_tile_height;

    switch (size) {
        case Sprite::Size::w8_h8: {
            int base_x = (texture_index / 8) * 16;
            base_x += 8 * (texture_index % 2);
            int base_y = ((texture_index % 8) / 2) * 8;
            src.x = base_x;
            src.y = base_y;
            src.w = 8;
            src.h = 8;
            break;
        }
        case Sprite::Size::w16_h16: {
            int base_x = (texture_index / 2) * 16;
            src.x = base_x;
            src.y = 16 * (texture_index % 2);
            src.w = 16;
            src.h = 16;
            break;
        }
        case Sprite::Size::w16_h32:
            src.x = base_x;
            src.y = base_y;
            src.w = 16;
            src.h = 32;
            break;
        case Sprite::Size::w32_h32:
            src.x = base_x * 2;
            src.y = base_y;
            src.w = 32;
            src.h = 32;
            break;
    }

    return src;
}



static double sprite_rotation_to_degrees(s16 rotation)
{
    // The GBA code uses -(INT16_MAX / 360) * degrees
    // So rotation ranges from 0 to 32767 for a full 360° rotation
    // We need to convert back to degrees
    return (rotation * 360.0) / 32767.0;
}



void Platform::Screen::draw(const Sprite& spr)
{
    if (spr.get_alpha() == Sprite::Alpha::transparent) {
        return;
    }

    auto pos = ivec(spr.get_position()) - spr.get_origin().cast<s32>();

    auto view_center = get_view().int_center().cast<s32>();
    pos = pos - view_center;
    pos.y = wrap_y(pos.y);

    ColorConstant color = ColorConstant::null;
    switch (spr.get_texture_index() % 8) {
        case 0: color = custom_color(0xFF0000); break;
        case 1: color = custom_color(0x00FF00); break;
        case 2: color = custom_color(0x0000FF); break;
        case 3: color = custom_color(0xFFFF00); break;
        case 4: color = custom_color(0xFF00FF); break;
        case 5: color = custom_color(0x00FFFF); break;
        case 6: color = custom_color(0xFFFFFF); break;
        case 7: color = custom_color(0x808080); break;
    }

    // Convert GBA scale to SDL scale
    // GBA: pa() = (1 << 8) - sx
    // The actual scale is 256 / pa()
    double scale_x = 1.0;
    double scale_y = 1.0;

    if (spr.get_scale().x != 0 || spr.get_scale().y != 0) {
        double pa = 256.0 - spr.get_scale().x;
        double pd = 256.0 - spr.get_scale().y;
        scale_x = 256.0 / pa;
        scale_y = 256.0 / pd;
    }

    sprite_draw_list.push_back({
        pos,
        spr.get_size(),
        spr.get_mix().color_,
        true,
        spr.get_texture_index(),
        spr.get_flip(),
        sprite_rotation_to_degrees(-spr.get_rotation()),
        spr.get_priority(),
        spr.get_alpha(),
        spr.get_mix().amount_,
        {scale_x, scale_y}
    });
}



void Platform::Screen::clear()
{
    sprite_draw_list.clear();

    // Clear entire screen to black first (for letterboxing)
    SDL_SetRenderDrawColor(renderer, 0, 0, 16, 255);
    SDL_RenderClear(renderer);

    auto bkg_color = current_shader(ShaderPalette::background,
                                    custom_color(0x63b2e0),
                                    0,
                                    1);

    auto clr = color_to_sdl(bkg_color);
    SDL_SetRenderDrawColor(renderer, clr.r, clr.g, clr.b, clr.a);
    SDL_Rect game_area = {0, 0, 240, 160};
    SDL_RenderFillRect(renderer, &game_area);
}



static SDL_Rect get_overlay_tile_source_rect(TileDesc tile_index)
{
    SDL_Rect src;

    if (!current_overlay_texture) {
        src.x = 0;
        src.y = 0;
        src.w = 8;
        src.h = 8;
        return src;
    }

    // Overlay tiles are 8x8 pixels
    const int tile_size = 8;
    const int tiles_per_row = overlay_texture_width / tile_size;

    int tile_x = tile_index % tiles_per_row;
    int tile_y = tile_index / tiles_per_row;

    src.x = tile_x * tile_size;
    src.y = tile_y * tile_size;
    src.w = tile_size;
    src.h = tile_size;

    return src;
}



static float last_fade_amt;
static ColorConstant fade_color;
static bool fade_include_overlay;


void Platform::Screen::fade(float amount,
                            ColorConstant k,
                            Optional<ColorConstant> base,
                            bool include_sprites,
                            bool include_overlay)
{
    last_fade_amt = amount;
    fade_color = k;
    fade_include_overlay = include_overlay;
}



bool Platform::Screen::fade_active() const
{
    return last_fade_amt > 0;
}



void Platform::Screen::schedule_fade(Float amount,
                                     const FadeProperties& props)
{
    last_fade_amt = amount;
    fade_color = props.color;
    fade_include_overlay = props.include_overlay;
}



void display_fade()
{
    if (last_fade_amt == 0) {
        return;
    }

    SDL_Rect rect;
    rect.x = 0;
    rect.y = 0;
    rect.w = 240;
    rect.h = 160;

    auto color = color_to_sdl(fade_color);
    SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_BLEND);
    SDL_SetRenderDrawColor(renderer, color.r, color.g, color.b, 255 * last_fade_amt);
    SDL_RenderFillRect(renderer, &rect);
    SDL_SetRenderDrawBlendMode(renderer, SDL_BLENDMODE_NONE);
}



void draw_sprite_group(int prio)
{
    // Draw all sprites as colored rectangles or textured
    for (const auto& sprite : reversed(sprite_draw_list)) {
        if (sprite.priority not_eq prio) {
            continue;
        }
        if (!sprite.visible) continue;

        if (!current_sprite_texture) {
            // Fallback to colored rectangles if no texture loaded
            SDL_Rect rect;
            rect.x = sprite.position.x;
            rect.y = sprite.position.y;

            // Set size based on sprite size enum
            switch (sprite.size) {
                case Sprite::Size::w8_h8:
                    rect.w = 8;
                    rect.h = 8;
                    break;
                case Sprite::Size::w16_h16:
                    rect.w = 16;
                    rect.h = 16;
                    break;
                case Sprite::Size::w16_h32:
                    rect.w = 16;
                    rect.h = 32;
                    break;
                case Sprite::Size::w32_h32:
                    rect.w = 32;
                    rect.h = 32;
                    break;
            }

            // Set color and draw
            auto color = color_to_sdl(sprite.color);
            SDL_SetRenderDrawColor(renderer, color.r, color.g, color.b, color.a);
            SDL_RenderFillRect(renderer, &rect);

            // Draw border for visibility
            SDL_SetRenderDrawColor(renderer, 0, 0, 0, 255);
            SDL_RenderDrawRect(renderer, &rect);
        } else {
            SDL_Rect src = get_sprite_source_rect(sprite.texture_index, sprite.size);

            SDL_Rect dst;
            dst.w = src.w * sprite.scale.x;  // Apply scale
            dst.h = src.h * sprite.scale.y;  // Apply scale
            dst.x = sprite.position.x - (dst.w - src.w) / 2;
            dst.y = sprite.position.y - (dst.h - src.h) / 2;

            // Handle flipping
            SDL_RendererFlip flip = SDL_FLIP_NONE;
            if (sprite.flip.x && sprite.flip.y) {
                flip = (SDL_RendererFlip)(SDL_FLIP_HORIZONTAL | SDL_FLIP_VERTICAL);
            } else if (sprite.flip.x) {
                flip = SDL_FLIP_HORIZONTAL;
            } else if (sprite.flip.y) {
                flip = SDL_FLIP_VERTICAL;
            }

            // Calculate base alpha
            u8 base_alpha = (sprite.alpha == Sprite::Alpha::translucent) ? 127 : 255;

            if (sprite.color != ColorConstant::null && sprite.mix_amount > 0) {
                // Dual-pass rendering with scaling
                float mix_ratio = sprite.mix_amount / 255.0f;

                SDL_SetTextureColorMod(current_sprite_texture, 255, 255, 255);
                SDL_SetTextureAlphaMod(current_sprite_texture, base_alpha * (1.0f - mix_ratio));
                SDL_RenderCopyEx(renderer, current_sprite_texture, &src, &dst,
                                sprite.rotation, nullptr, flip);

                auto mix_color = color_to_sdl(sprite.color);
                SDL_SetTextureColorMod(current_sprite_texture, mix_color.r, mix_color.g, mix_color.b);
                SDL_SetTextureAlphaMod(current_sprite_texture, base_alpha * mix_ratio);
                SDL_RenderCopyEx(renderer, current_sprite_texture, &src, &dst,
                                sprite.rotation, nullptr, flip);

                SDL_SetTextureColorMod(current_sprite_texture, 255, 255, 255);
                SDL_SetTextureAlphaMod(current_sprite_texture, 255);
            } else {
                SDL_SetTextureColorMod(current_sprite_texture, 255, 255, 255);
                SDL_SetTextureAlphaMod(current_sprite_texture, base_alpha);

                SDL_RenderCopyEx(renderer, current_sprite_texture, &src, &dst,
                                sprite.rotation, nullptr, flip);

                SDL_SetTextureAlphaMod(current_sprite_texture, 255);
            }
        }
    }
}



static void draw_tile_layer(Layer layer, SDL_Texture* texture, int texture_width,
                            const Vec2<s32>& scroll, const View& view)
{
    if (!texture) {
        return;
    }

    auto& tiles = tile_layers_[layer];
    if (tiles.empty()) {
        return;
    }

    bool skip_tile_zero = true;
    if (layer == Layer::map_0_ext || layer == Layer::map_0) {
        skip_tile_zero = tile0_index_zero_is_transparent;
    } else if (layer == Layer::map_1_ext || layer == Layer::map_1) {
        skip_tile_zero = tile1_index_zero_is_transparent;
    } else if (layer == Layer::overlay) {
        skip_tile_zero = overlay_index_zero_is_transparent;
    }

    auto view_center = view.int_center().cast<s32>();
    bool is_ext_layer = (layer == Layer::map_0_ext || layer == Layer::map_1_ext);

    for (auto& [pos, tile_info] : tiles) {
        if (skip_tile_zero and tile_info.tile_desc == 0) continue;

        s32 tile_x = (s32)pos.first;
        s32 tile_y = (s32)pos.second;

        SDL_Rect src;
        SDL_Rect dst;

        if (is_ext_layer) {
            src = get_tile_source_rect_16x16(tile_info.tile_desc, texture_width);
            dst.x = tile_x * 16 - scroll.x - view_center.x;
            dst.y = wrap_y(tile_y * 16 - scroll.y - view_center.y);
            dst.w = 16;
            dst.h = 16;
        } else {
            src = get_tile_source_rect_8x8(tile_info.tile_desc, texture_width);
            dst.x = tile_x * 8 - scroll.x - view_center.x;
            dst.y = wrap_y(tile_y * 8 - scroll.y - view_center.y);
            dst.w = 8;
            dst.h = 8;
        }

        // Check if this tile uses a special palette (13, 14, or 15)
        auto special_it = special_palettes.find(tile_info.palette);
        if (special_it != special_palettes.end()) {
            // Render to intermediate buffer
            SDL_SetRenderTarget(renderer, tile_recolor_buffer);

            // Fill buffer with the solid color
            SDL_SetRenderDrawColor(renderer,
                                  special_it->second.r,
                                  special_it->second.g,
                                  special_it->second.b,
                                  0);  // Alpha 0 for now
            SDL_RenderClear(renderer);

            // Copy ONLY the alpha channel from the tile
            // Draw the tile but use a custom blend mode that only copies alpha
            SDL_Rect temp_dst = {0, 0, dst.w, dst.h};

            // Set color mod to black so RGB doesn't affect anything
            SDL_SetTextureColorMod(texture, 0, 0, 0);
            SDL_SetTextureAlphaMod(texture, 255);

            // Use a blend mode that copies the source alpha to destination alpha
            SDL_SetTextureBlendMode(texture, SDL_ComposeCustomBlendMode(
                SDL_BLENDFACTOR_ZERO,           // srcColorFactor (ignore source RGB)
                SDL_BLENDFACTOR_ONE,            // dstColorFactor (keep our solid color)
                SDL_BLENDOPERATION_ADD,         // colorOperation
                SDL_BLENDFACTOR_ONE,            // srcAlphaFactor (use source alpha)
                SDL_BLENDFACTOR_ZERO,           // dstAlphaFactor (replace dest alpha)
                SDL_BLENDOPERATION_ADD          // alphaOperation
            ));

            SDL_RenderCopy(renderer, texture, &src, &temp_dst);

            // Reset texture settings
            SDL_SetTextureColorMod(texture, 255, 255, 255);
            SDL_SetTextureAlphaMod(texture, 255);
            SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_BLEND);

            // Switch back to main screen
            SDL_SetRenderTarget(renderer, nullptr);

            // Draw the recolored tile to screen
            SDL_RenderCopy(renderer, tile_recolor_buffer, &temp_dst, &dst);
        } else {
            SDL_RenderCopy(renderer, texture, &src, &dst);
        }
    }
}



ColorConstant bg_color_default()
{
    // ...
    return ColorConstant::rich_black;
}



void SDL_RenderFillCircle(SDL_Renderer *renderer,
                          int cx,
                          int cy,
                          int radius,
                          Uint8 r,
                          Uint8 g,
                          Uint8 b,
                          Uint8 a) {
    SDL_SetRenderDrawColor(renderer, r, g, b, a);
    for (int y = -radius; y <= radius; y++) {
        int width = (int)sqrt(radius*radius - y*y);
        SDL_RenderDrawLine(renderer, cx - width, cy + y, cx + width, cy + y);
    }
}



void Platform::Screen::display()
{
    draw_sprite_group(3);
    draw_tile_layer(Layer::map_1_ext, tile1_texture, tile1_texture_width,
                   tile1_scroll, get_view());
    draw_tile_layer(Layer::map_1, tile1_texture, tile1_texture_width,
                   tile1_scroll, get_view());
    // Draw tile0 layer
    draw_tile_layer(Layer::map_0_ext, tile0_texture, tile0_texture_width,
                   tile0_scroll, get_view());
    draw_tile_layer(Layer::map_0, tile0_texture, tile0_texture_width,
                   tile0_scroll, get_view());
    if (tile0_scroll.x > 240 and tile0_scroll.x < 512) {
        // Hack to account for gba scroll wrapping, in the inteval between where
        // the x coordinate of a scrolled layer wraps.
        auto scroll_tmp = tile0_scroll;
        scroll_tmp.x -= 512;
        draw_tile_layer(Layer::map_0, tile0_texture, tile0_texture_width,
                        scroll_tmp, get_view());
    }
    draw_sprite_group(2);
    draw_sprite_group(1);

    if (not fade_include_overlay) {
        display_fade();
    }

    if (circle_effect_radius) {
        ColorConstant color = custom_color(0xceb282);
        if (circle_effect_radius <= 70) {
            color = custom_color(0xca7f5c);
        }
        auto clr = color_to_sdl(color);
        SDL_RenderFillCircle(renderer,
                             circle_effect_origin_x,
                             circle_effect_origin_y,
                             circle_effect_radius,
                             clr.r,
                             clr.g,
                             clr.b,
                             255);
    } else if (current_overlay_texture) {
        auto& overlay_tiles = tile_layers_[Layer::overlay];
        for (auto& [pos, tile_info] : overlay_tiles) {
            auto tile_desc = tile_info.tile_desc;
            if (tile_desc == 0) continue;

            SDL_Rect src = get_overlay_tile_source_rect(tile_desc);
            SDL_Rect dst;
            dst.x = pos.first * 8 - overlay_origin.x;
            dst.y = pos.second * 8 - overlay_origin.y;
            dst.w = 8;
            dst.h = 8;

            // Draw background color for glyphs
            if (is_glyph(tile_desc)) {
                ColorConstant bg_color_key;
                bool skip = false;
                if ((int)tile_info.text_bg_color_ != 0) {
                    bg_color_key = tile_info.text_bg_color_;
                } else {
                    bg_color_key = (ColorConstant)((default_text_bg_color.r << 16) |
                                                   (default_text_bg_color.g << 8) |
                                                   default_text_bg_color.b);
                    skip = default_text_bg_color.a == 0;
                }

                if (not skip) {
                    auto color = color_to_sdl(bg_color_key);
                    SDL_SetRenderDrawColor(renderer, color.r, color.g, color.b, 255);
                    SDL_RenderFillRect(renderer, &dst);
                }
            }

            // Apply foreground color if specified, otherwise use default
            if ((int)tile_info.text_fg_color_ != 0) {
                auto fg_color = color_to_sdl(tile_info.text_fg_color_);
                SDL_SetTextureColorMod(current_overlay_texture, fg_color.r, fg_color.g, fg_color.b);
            } else if (is_glyph(tile_desc)) {
                // Use extracted default foreground color for glyphs
                SDL_SetTextureColorMod(current_overlay_texture,
                                      default_text_fg_color.r,
                                      default_text_fg_color.g,
                                      default_text_fg_color.b);
            }

            SDL_RenderCopy(renderer, current_overlay_texture, &src, &dst);

            // Reset color mod
            SDL_SetTextureColorMod(current_overlay_texture, 255, 255, 255);
        }
    }

    draw_sprite_group(0);

    if (fade_include_overlay) {
        display_fade();
    }

    SDL_RenderPresent(renderer);
    sprite_draw_list.clear();
}



Vec2<u32> Platform::Screen::size() const
{
    return {240, 160};
}



void Platform::Screen::pixelate(u8 amount,
                                bool include_overlay,
                                bool include_background,
                                bool include_sprites)
{
}



////////////////////////////////////////////////////////////////////////////////
// Speaker
////////////////////////////////////////////////////////////////////////////////



StringBuffer<48> Platform::Speaker::current_music()
{
    return "";
}



bool Platform::Speaker::stream_music(const char* filename, Microseconds offset)
{
    return true; // FIXME!!!
}



void Platform::Speaker::play_music(const char* name, Microseconds offset)
{
}



Platform::Speaker::Speaker()
{
}



void Platform::Speaker::set_music_speed(MusicSpeed speed)
{
}



void Platform::Speaker::set_music_volume(u8 volume)
{
}



void Platform::Speaker::set_sounds_volume(u8 volume)
{
}



void Platform::Speaker::stash_sounds()
{
}



void Platform::Speaker::restore_sounds()
{
}



void Platform::Speaker::start()
{
}



Microseconds Platform::Speaker::track_length(const char* name)
{
    return 1;
}



bool Platform::Speaker::is_sound_playing(const char* name)
{
    return false;
}



bool Platform::Speaker::is_music_playing(const char* name)
{
    return false;
}



Buffer<const char*, 4> Platform::Speaker::completed_sounds()
{
    return {};
}



void Platform::Speaker::stop_sound(const char* name)
{
}



void Platform::Speaker::clear_sounds()
{
}



void Platform::Speaker::play_sound(const char* name,
                                   int priority,
                                   Optional<Vec2<Float>> position)
{
}



void Platform::Speaker::stop_music()
{
}



////////////////////////////////////////////////////////////////////////////////
// Platform impl
////////////////////////////////////////////////////////////////////////////////



Platform::Platform()
{
    update_viewport();
    constrain_window_to_scale();

    std::ifstream in(save_file_name, std::ios_base::in | std::ios_base::binary);
    if (in) {
        in.read((char*)save_buffer, ::save_capacity);
    }
}



Platform::~Platform()
{
}



bool Platform::is_running() const
{
    return sdl_running;
}



Platform::DeviceName Platform::device_name() const
{
    return "PC";
}



Platform::ModelName Platform::model_name() const
{
#if defined(_WIN32) or defined(_WIN64)
    return "Windows";
#elif defined(__APPLE__)
    return "Mac";
#else
    return "Linux";
#endif
}



void Platform::memset_words(void* dest, u8 byte, u32 word_count)
{
    memset(dest, byte, word_count * sizeof(void*));
}



void no_op_task()
{
}


Platform::TaskPointer Platform::set_background_task(Platform::TaskPointer task)
{
    return no_op_task;
}
