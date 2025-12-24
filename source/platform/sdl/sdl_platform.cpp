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



Platform* __platform__ = nullptr;



int main(int argc, char** argv)
{
    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        fprintf(stderr, "SDL failed to initialise: %s\n", SDL_GetError());
        return 1;
    }

    window = SDL_CreateWindow("Skyland",
                              SDL_WINDOWPOS_UNDEFINED,
                              SDL_WINDOWPOS_UNDEFINED,
                              480,  // 240 * 2 for scaling
                              320,  // 160 * 2
                              SDL_WINDOW_SHOWN);

    if (window == NULL) {
        fprintf(stderr, "SDL window failed to initialise: %s\n", SDL_GetError());
        return 1;
    }

    // Create renderer instead of getting surface
    renderer = SDL_CreateRenderer(window, -1,
                                  SDL_RENDERER_ACCELERATED |
                                  SDL_RENDERER_PRESENTVSYNC);

    if (renderer == NULL) {
        fprintf(stderr, "SDL renderer failed to initialise: %s\n", SDL_GetError());
        return 1;
    }

    // Enable integer scaling for crisp pixels
    SDL_RenderSetIntegerScale(renderer, SDL_TRUE);
    SDL_RenderSetLogicalSize(renderer, 240, 160);

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



std::map<Layer, std::map<std::pair<u16, u16>, TileDesc>> tile_layers_;



TileDesc Platform::get_tile(Layer layer, u16 x, u16 y)
{
    return tile_layers_[layer][{x, y}];
}



void Platform::set_tile(Layer layer,
                        u16 x,
                        u16 y,
                        TileDesc val,
                        Optional<u16> palette)
{
    switch (layer) {
    case Layer::overlay:
        if (x > 31 or y > 31) {
            return;
        }
        tile_layers_[layer][{x, y}] = val;
        break;

    case Layer::map_0_ext:
    case Layer::map_0:
    case Layer::map_1_ext:
    case Layer::map_1:
    case Layer::background:
        tile_layers_[layer][{x, y}] = val;
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

        case SDL_KEYDOWN: {
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



void Platform::DynamicTexture::remap(u16 spritesheet_offset)
{
    // No need to do anything on the desktop platform. All of our texture fits
    // in vram, so we do not need to manage any "virtual" tile indices.
}



Optional<Platform::DynamicTexturePtr> Platform::make_dynamic_texture()
{
    return std::nullopt;
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



Vec2<u16> Platform::get_scroll(Layer layer)
{
    return {};
}



void Platform::set_scroll(Layer layer, u16 x, u16 y)
{
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



u16 Platform::get_palette(Layer layer, u16 x, u16 y)
{
    return 0;
}



void Platform::set_palette(Layer layer, u16 x, u16 y, u16 palette)
{
}



void Platform::set_raw_tile(Layer layer, u16 x, u16 y, TileDesc val)
{
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



TileDesc Platform::map_tile0_chunk(TileDesc src)
{
    return 0;
}



TileDesc Platform::map_tile1_chunk(TileDesc src)
{
    return 0;
}



void Platform::walk_filesystem(
    Function<8 * sizeof(void*), void(const char* path)> callback)
{
    // TODO...
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

    // Set magenta as transparent
    Uint32 color_key = SDL_MapRGB(surface->format, 0xFF, 0x00, 0xFF);
    SDL_SetColorKey(surface, SDL_TRUE, color_key);

    charset_surfaces[texture_name] = surface;

    info(format("Loaded charset texture %", texture_name));

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
            tile_layers_[Layer::overlay][{x, y}] = tile_desc;
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
}



void Platform::override_priority(Layer layer, int priority)
{
    // TODO...
}



static SDL_Texture* load_png_texture(const char* filename)
{
    std::string full_path = resource_path() + "images" + PATH_DELIMITER + filename + ".png";

    SDL_Surface* surface = IMG_Load(full_path.c_str());
    if (!surface) {
        error(format("Failed to load image %: %", full_path.c_str(), IMG_GetError()));
        return nullptr;
    }

    // Set magenta (0xFF00FF) as the transparent color key
    // SDL wants RGB in the surface's format, so we need to map the color
    Uint32 color_key = SDL_MapRGB(surface->format, 0xFF, 0x00, 0xFF);
    SDL_SetColorKey(surface, SDL_TRUE, color_key);

    SDL_Texture* texture = SDL_CreateTextureFromSurface(renderer, surface);
    if (!texture) {
        error(format("Failed to create texture from %: %", full_path.c_str(), SDL_GetError()));
        SDL_FreeSurface(surface);
        return nullptr;
    }

    // Enable alpha blending on the texture
    SDL_SetTextureBlendMode(texture, SDL_BLENDMODE_BLEND);

    int w, h;
    SDL_QueryTexture(texture, nullptr, nullptr, &w, &h);
    info(format("Loaded sprite texture %: %x% pixels", filename, w, h));

    SDL_FreeSurface(surface);
    return texture;
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



void Platform::load_overlay_chunk(TileDesc dst,
                                  TileDesc src,
                                  u16 count,
                                  const char* image_file)
{
}



void Platform::load_tile0_texture(const char* name)
{
}



void Platform::load_tile1_texture(const char* name)
{
}



Vec2<Float> overlay_origin;



void Platform::set_overlay_origin(Float x, Float y)
{
    overlay_origin = {x, y};
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

    current_sprite_texture = load_png_texture(name);
    if (current_sprite_texture) {
        SDL_QueryTexture(current_sprite_texture, nullptr, nullptr,
                        &sprite_texture_width, &sprite_texture_height);
    }
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


StringBuffer<256> last_overlay_texture;
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
    info(format("Raw loaded surface: %x% pixels", loaded_surface->w, loaded_surface->h));

    if (loaded_surface->w == 0 || loaded_surface->h == 0) {
        error(format("Overlay % has invalid dimensions: %x%",
                     name, loaded_surface->w, loaded_surface->h));
        SDL_FreeSurface(loaded_surface);
        return false;
    }


    // Convert indexed color to RGBA32 if needed
    SDL_Surface* converted_surface = loaded_surface;
    if (loaded_surface->format->palette != nullptr) {
        info("Converting indexed color surface to RGBA");
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
    // This is 504 tiles of 8x8 pixels (4032 / 8 = 504)
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

    info(format("Loaded overlay texture %: %x% pixels (clamped from %x%), % tiles",
                name, overlay_texture_width, overlay_texture_height,
                loaded_surface->w, overlay_base_tile_count));

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
}



void Platform::RemoteConsole::start()
{
    std::thread console_thread(console_thread_main);
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
}



void Platform::Screen::set_shader_argument(int arg)
{
}



struct SpriteDrawInfo {
    Vec2<s32> position;
    Sprite::Size size;
    ColorConstant color;
    bool visible;
    u16 texture_index;
    Vec2<bool> flip;
    s16 rotation;
};


static std::vector<SpriteDrawInfo> sprite_draw_list;



void Platform::Screen::draw_batch(TextureIndex t,
                                  const Buffer<Vec2<s32>, 64>& coords,
                                  const SpriteBatchOptions& opts)
{
}



static SDL_Rect get_sprite_source_rect(u16 texture_index, Sprite::Size size)
{
    SDL_Rect src;

    // GBA sprites are organized in 16x32 pixel meta-tiles
    // Each meta-tile is 16 pixels wide and 32 pixels tall
    const int meta_tile_width = 16;
    const int meta_tile_height = 32;

    // Calculate which meta-tile this texture index refers to
    // Meta-tiles are laid out left-to-right, top-to-bottom in the sprite sheet
    int tiles_per_row = sprite_texture_width / meta_tile_width;
    int meta_tile_x = texture_index % tiles_per_row;
    int meta_tile_y = texture_index / tiles_per_row;

    // Base position of the meta-tile
    int base_x = meta_tile_x * meta_tile_width;
    int base_y = meta_tile_y * meta_tile_height;

    switch (size) {
        case Sprite::Size::w8_h8: {
            // 8x8 sprites are packed 2 across, 4 down within a 16x32 meta-tile
            // Layout within meta-tile:
            // [0][1]
            // [2][3]
            // [4][5]
            // [6][7]
            int sub_index = texture_index % 8; // which 8x8 sprite within the meta-tile
            int sub_x = (sub_index % 2) * 8;
            int sub_y = (sub_index / 2) * 8;

            src.x = base_x + sub_x;
            src.y = base_y + sub_y;
            src.w = 8;
            src.h = 8;
            break;
        }

        case Sprite::Size::w16_h16: {
            // 16x16 sprites are packed 2 vertically within a 16x32 meta-tile
            int sub_index = texture_index % 2;
            int sub_y = sub_index * 16;

            src.x = base_x;
            src.y = base_y + sub_y;
            src.w = 16;
            src.h = 16;
            break;
        }

        case Sprite::Size::w16_h32:
            // 16x32 is exactly one meta-tile
            src.x = base_x;
            src.y = base_y;
            src.w = 16;
            src.h = 32;
            break;

        case Sprite::Size::w32_h32: {
            // 32x32 sprites span two meta-tiles horizontally
            src.x = base_x * 2;
            src.y = base_y;
            src.w = 32;
            src.h = 32;
            break;
        }
    }

    return src;
}



void Platform::Screen::draw(const Sprite& spr)
{
    if (spr.get_alpha() == Sprite::Alpha::transparent) {
        return;
    }

    auto pos = ivec(spr.get_position()) - spr.get_origin().cast<s32>();

    auto view_center = get_view().int_center().cast<s32>();
    pos = pos - view_center;

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

    sprite_draw_list.push_back({
        pos,
        spr.get_size(),
        color,
        true,
        spr.get_texture_index(),
        spr.get_flip(),
        spr.get_rotation()
    });
}



void Platform::Screen::clear()
{
    sprite_draw_list.clear();

    SDL_SetRenderDrawColor(renderer, 255, 0, 255, 255);
    SDL_RenderClear(renderer);
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



void Platform::Screen::fade(float amount,
                            ColorConstant k,
                            Optional<ColorConstant> base,
                            bool include_sprites,
                            bool include_overlay)
{
    last_fade_amt = amount;
    fade_color = k;
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


void Platform::Screen::display()
{
    display_fade();

    if (current_overlay_texture) {
        auto& overlay_tiles = tile_layers_[Layer::overlay];

        for (auto& [pos, tile_desc] : overlay_tiles) {
            if (tile_desc == 0) continue; // Skip empty tiles

            SDL_Rect src = get_overlay_tile_source_rect(tile_desc);
            SDL_Rect dst;
            dst.x = pos.first * 8 - overlay_origin.x;
            dst.y = pos.second * 8 - overlay_origin.y;
            dst.w = 8;
            dst.h = 8;

            SDL_RenderCopy(renderer, current_overlay_texture, &src, &dst);
        }
    }

    // Draw all sprites as colored rectangles or textured
    for (const auto& sprite : sprite_draw_list) {
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
            // Draw textured sprite
            SDL_Rect src = get_sprite_source_rect(sprite.texture_index, sprite.size);

            SDL_Rect dst;
            dst.x = sprite.position.x;
            dst.y = sprite.position.y;
            dst.w = src.w;
            dst.h = src.h;

            // Handle flipping
            SDL_RendererFlip flip = SDL_FLIP_NONE;
            if (sprite.flip.x && sprite.flip.y) {
                flip = (SDL_RendererFlip)(SDL_FLIP_HORIZONTAL | SDL_FLIP_VERTICAL);
            } else if (sprite.flip.x) {
                flip = SDL_FLIP_HORIZONTAL;
            } else if (sprite.flip.y) {
                flip = SDL_FLIP_VERTICAL;
            }

            // Render the sprite
            SDL_RenderCopyEx(renderer, current_sprite_texture, &src, &dst,
                           sprite.rotation, nullptr, flip);
        }
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



static Platform::Extensions extensions;



const Platform::Extensions& Platform::get_extensions()
{
    return extensions;
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
